{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Regex.KDE.Compile
  (compileRegex)
  where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Safe
import Data.Attoparsec.Text as A hiding (match)
import Data.Char
import Control.Applicative
import Regex.KDE.Regex
import Control.Monad
import Control.Monad.State.Strict
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

-- I believe the Regex engine used in KatePart is Qt's.
-- It is described here: https://doc.qt.io/qt-6/qregexp.html

-- | Compile a UTF-8 encoded ByteString as a Regex.  If the first
-- parameter is True, then the Regex will be case sensitive.
compileRegex :: Bool -> ByteString -> Either String Regex
compileRegex caseSensitive bs =
  let !res = parseOnly (evalStateT parser RState{
                                            rsCurrentCaptureNumber = 0,
                                            rsCaseSensitive = caseSensitive })
                       (decodeUtf8With lenientDecode bs)
   in res
 where
   parser = do
     !re <- pRegex
     (re <$ lift A.endOfInput) <|>
       do rest <- lift A.takeText
          fail $ "parse error at byte position " ++
                 show (B.length bs - B.length (encodeUtf8 rest))

data RState =
  RState
  { rsCurrentCaptureNumber :: Int
  , rsCaseSensitive :: Bool }
  deriving (Show)

type RParser = StateT RState Parser

pRegex :: RParser Regex
pRegex =
  option MatchNull $
  -- earlier alternatives must be the left operands of MatchAlt, since
  -- the matcher prefers them, as in PCRE.  The first alternative may
  -- be empty, as in (?:|a); as in PCRE, an empty alternative matches
  -- the empty string:
  (\x xs -> foldr1 MatchAlt (x:xs))
    <$> (pAltPart <|> pure mempty)
    <*> many (lift (char '|') *> (pAltPart <|> pure mempty))

pAltPart :: RParser Regex
pAltPart = mconcat <$> many1 pRegexPart

pRegexPart :: RParser Regex
pRegexPart =
  pRegexChar <|> pParenthesized >>= pSuffix

pParenthesized :: RParser Regex
pParenthesized = do
  _ <- lift (char '(')
  pInlineModifiers <|> do
    -- pcrepattern says: A group that starts with (?| resets the capturing
    -- parentheses numbers in each alternative.
    resetCaptureNumbers <- option False (True <$ lift (string "?|"))
    (modifier, stModifier) <-
                if resetCaptureNumbers
                   then return (id, id)
                   else lift (char '?' *> pGroupModifiers)
                      <|> do modify (\st -> st{
                                        rsCurrentCaptureNumber =
                                               rsCurrentCaptureNumber st + 1})
                             num <- gets rsCurrentCaptureNumber
                             pure (MatchCapture num, id)
    currentCaptureNumber <- gets rsCurrentCaptureNumber
    -- modifiers like (?i: are scoped to the group, so save the current
    -- case sensitivity and restore it after the closing parenthesis:
    oldCaseSensitive <- gets rsCaseSensitive
    modify stModifier
    contents <- do
      x <- pAltPart <|> pure mempty
      n0 <- gets rsCurrentCaptureNumber
      let pNextAlt = do
            _ <- lift (char '|')
            when resetCaptureNumbers $
              modify (\st ->
                       st{ rsCurrentCaptureNumber = currentCaptureNumber })
            y <- pAltPart <|> pure mempty
            n <- gets rsCurrentCaptureNumber
            pure (y, n)
      rest <- many pNextAlt
      -- with (?|, numbering after the group resumes after the highest
      -- group number used in any alternative, as in PCRE:
      when resetCaptureNumbers $
        modify (\st ->
                 st{ rsCurrentCaptureNumber = maximum (n0 : map snd rest) })
      pure (foldr1 MatchAlt (x : map fst rest))
    _ <- lift (char ')')
    modify $ \st -> st{ rsCaseSensitive = oldCaseSensitive }
    return $ modifier contents

-- Inline modifiers like (?i) or (?-i), without a colon, apply from
-- this point to the end of the enclosing group (or pattern).  The
-- state change persists after the closing parenthesis; the enclosing
-- group's save/restore of rsCaseSensitive provides the scoping.
pInlineModifiers :: RParser Regex
pInlineModifiers = do
  stModifier <- lift $ char '?' *> pRegexModifier <* char ')'
  modify stModifier
  return MatchNull

pGroupModifiers :: Parser (Regex -> Regex, RState -> RState)
pGroupModifiers =
  (do stmod <- pRegexModifier -- (?i:
      void (char ':')
      pure (id, stmod))
   <|>
     do dir <- option Forward $ Backward <$ char '<'
        ((AssertPositive dir, id) <$ char '=') <|>
          ((AssertNegative dir, id) <$ char '!')
   <|>
     do ds <- many1 digit
        case readMay ds of
          Just !n -> return (\_ -> Subroutine n, id)
          Nothing -> fail "not a number"
   <|>
     do void $ char 'R'
        return  (\_ -> Subroutine 0, id)
   <|> -- atomic group (?>...): no backtracking into the group
     ((Possessive, id) <$ char '>')

pRegexModifier :: Parser (RState -> RState)
pRegexModifier = do
  -- "adlupimnsx-imnsx"
  -- i = 105  - = 45
  ons <- many $ satisfy (inClass "adlupimnsx")
  offs <- option [] $ char '-' *>
                      many (satisfy (inClass "imnsx"))
  pure $ \st -> st{
    rsCaseSensitive =
      if 'i' `elem` ons && 'i' `notElem` offs
         then False
         else ('i' `elem` offs) || rsCaseSensitive st
  }

pSuffix :: Regex -> RParser Regex
pSuffix re = option re $ do
  w <- lift $ satisfy (inClass "*+?{")
  case w of
    '*'  -> withModifier (MatchAlt (MatchSome re) MatchNull)
                         (MatchAlt MatchNull (Lazy (MatchSome re)))
    '+'  -> withModifier (MatchSome re) (Lazy (MatchSome re))
    '?'  -> withModifier (MatchAlt re MatchNull) (MatchAlt MatchNull re)
    '{'  -> do
      minn <- lift $
        option Nothing $ readMay . T.unpack <$> A.takeWhile isDigit
      maxn <- lift $ option minn $ char ',' *>
                       (readMay . T.unpack <$> A.takeWhile isDigit)
      _ <- lift $ char '}'
      case (minn, maxn) of
          _ | maybe False (> maxRepeat) minn ||
              maybe False (> maxRepeat) maxn
                             -> mzero -- fall back to literal interpretation
          (Nothing, Nothing) -> mzero
          (Just n, Nothing)  -> withModifier (atleast n re) (atleastLazy n re)
          (Nothing, Just n)  -> withModifier (atmost n re) (atmostLazy n re)
          (Just m, Just n)
            | m > n          -> mzero -- invalid quantifier, e.g. a{3,1};
                                      -- fall back to literal interpretation
            | otherwise      -> withModifier (between m n re)
                                             (betweenLazy m n re)
    _   -> fail "pSuffix encountered impossible byte"
 where
   -- A lazy quantifier prefers fewer repetitions, which is expressed
   -- by putting the empty alternative first; Lazy itself is only ever
   -- applied to MatchSome (the matcher relies on this).  A possessive
   -- quantifier commits to the preferred match of the greedy version.
   withModifier greedy lazy = lift $
     (Possessive greedy <$ char '+') <|> (lazy <$ char '?') <|> pure greedy

   -- repeat counts larger than this (the limit PCRE2 uses) are not
   -- treated as quantifiers:
   maxRepeat = 65535 :: Int

   -- nest the optional matches -- r(r(r)?)? -- so that the size of
   -- the compiled regex is linear, not quadratic, in n:
   atmost n r
     | n <= 0 = MatchNull
     | otherwise = MatchAlt (r <> atmost (n - 1) r) MatchNull

   atmostLazy n r
     | n <= 0 = MatchNull
     | otherwise = MatchAlt MatchNull (r <> atmostLazy (n - 1) r)

   between 0 n r = atmost n r
   between m n r = mconcat (replicate m r) <> atmost (n - m) r

   betweenLazy 0 n r = atmostLazy n r
   betweenLazy m n r = mconcat (replicate m r) <> atmostLazy (n - m) r

   atleast n r = mconcat (replicate n r) <> MatchAlt (MatchSome r) MatchNull

   atleastLazy n r = mconcat (replicate n r) <>
                     MatchAlt MatchNull (Lazy (MatchSome r))

pRegexChar :: RParser Regex
pRegexChar = do
  w <- lift anyChar
  caseSensitive <- gets rsCaseSensitive
  case w of
    '.'  -> return MatchAnyChar
    '%' -> (do -- dynamic %1 %2
              ds <- lift $ many1 digit
              case readMay ds of
                Just !n -> return $ MatchDynamic n
                Nothing -> fail "not a number")
            <|> return (MatchChar (== '%'))
    '\\' -> lift $ pRegexEscapedChar caseSensitive
    '$'  -> return AssertEnd
    '^'  -> return AssertBeginning
    '['  -> lift $ pRegexCharClass caseSensitive
    _ | isSpecial w -> mzero
      | otherwise -> return $!
            MatchChar $ if caseSensitive
                           then (== w)
                           else (\d -> toLower d == toLower w)

pRegexEscapedChar :: Bool -> Parser Regex
pRegexEscapedChar caseSensitive = do
  c <- A.anyChar
  (case c of
    'b' -> return AssertWordBoundary
    'B' -> return $ AssertNegative Forward AssertWordBoundary
    -- PCRE's \G asserts the position at which the match attempt
    -- started.  Since matching is always anchored at the start of
    -- the input we are given, that is the same as AssertBeginning:
    'G' -> return AssertBeginning
    -- PCRE's \A asserts the start of the subject.  Since matching is
    -- always anchored at the start of the input we are given, that is
    -- also the same as AssertBeginning:
    'A' -> return AssertBeginning
    '{' -> do -- captured pattern: \1 \2 \{12}
              ds <- many1 digit
              _ <- char '}'
              case readMay ds of
                Just !n -> return $ MatchCaptured n caseSensitive
                Nothing -> fail "not a number"
    'g' -> do -- PCRE backreference syntax: \g1 \g{12}
              ds <- (char '{' *> many1 digit <* char '}') <|> many1 digit
              case readMay ds of
                Just !n -> return $ MatchCaptured n caseSensitive
                Nothing -> fail "not a number"
    'd' -> return $ MatchChar isDigit
    'D' -> return $ MatchChar (not . isDigit)
    's' -> return $ MatchChar isSpace
    'S' -> return $ MatchChar (not . isSpace)
    'h' -> return $ MatchChar isHorizSpace
    'H' -> return $ MatchChar (not . isHorizSpace)
    'w' -> return $ MatchChar isWordChar
    'W' -> return $ MatchChar (not . isWordChar)
    'p' -> MatchChar <$> pUnicodeCharClass
    _ | isDigit c, c /= '0' -> -- \0 is an octal escape, not a backreference
       return $! MatchCaptured (ord c - ord '0') caseSensitive
      | otherwise -> mzero) <|> (matchLiteralChar <$> pEscaped c)
 where
   matchLiteralChar d = MatchChar $
     if caseSensitive
        then (== d)
        else \x -> toLower x == toLower d

pEscaped :: Char -> Parser Char
pEscaped c =
  case c of
    '\\' -> return c
    'a' -> return '\a'
    -- \b means backspace inside a character class (outside one, it is
    -- a word boundary assertion handled by pRegexEscapedChar):
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'v' -> return '\v'
    '0' -> do -- \0 followed by up to two octal digits (as in PCRE)
      ds <- A.scan (0 :: Int) (\s w -> if s < 2 && isOctDigit w
                                          then Just (s + 1)
                                          else Nothing)
      case readMay ("'\\o0" ++ T.unpack ds ++ "'") of
        Just x  -> return x
        Nothing -> fail "invalid octal character escape"
    _ | c >= '1' && c <= '7' -> do
      -- \123 matches octal 123, \1 matches octal 1
      let octalDigitScanner s w
            | s < 3, isOctDigit w = Just (s + 1) -- digits 0-7
            | otherwise = Nothing
      ds <- A.scan (1 :: Int) octalDigitScanner
      case readMay ("'\\o" ++ [c] ++ T.unpack ds ++ "'") of
        Just x  -> return x
        Nothing -> fail "invalid octal character escape"
    'z' -> do -- \zhhhh matches unicode hex char hhhh
      ds <- A.take 4
      case readMay ("'\\x" ++ T.unpack ds ++ "'") of
        Just x  -> return x
        Nothing -> fail "invalid hex character escape"
    'x' -> do -- \xhh matches hex hh, \x{h+} matches hex h+
      ds <- (char '{' *> A.takeWhile (/= '}') <* char '}')
             <|> A.take 2
      case readMay ("'\\x" ++ T.unpack ds ++ "'") of
        Just x  -> return x
        Nothing -> fail "invalid hex character escape"
    _ | isPunctuation c || isSymbol c || isSpace c -> return c
      | otherwise -> fail $ "invalid escape \\" ++ [c]

pRegexCharClass :: Bool -> Parser Regex
pRegexCharClass caseSensitive = do
  negated <- option False $ True <$ char '^'
  let getEscapedClass = do
        _ <- char '\\'
        (isDigit <$ char 'd')
         <|> (not . isDigit <$ char 'D')
         <|> (isSpace <$ char 's')
         <|> (not . isSpace <$ char 'S')
         <|> (isHorizSpace <$ char 'h')
         <|> (not . isHorizSpace <$ char 'H')
         <|> (isWordChar <$ char 'w')
         <|> (not . isWordChar <$ char 'W')
  let getPosixClass = do
        _ <- string "[:"
        localNegated <- option False $ True <$ char '^'
        res <- (isAlphaNum <$ string "alnum")
             <|> (isAlpha <$ string "alpha")
             <|> (isAscii <$ string "ascii")
             <|> ((\c -> isSpace c && c `notElem` ['\n','\r','\f','\v']) <$
                   string "blank")
             <|> (isControl <$ string "cntrl")
             <|> (isDigit <$ string "digit")
             <|> ((\c -> isPrint c && not (isSpace c)) <$ string "graph")
             <|> (isLower <$ string "lower")
             <|> (isUpper <$ string "upper")
             <|> (isPrint <$ string "print")
             <|> (isPunctuation <$ string "punct")
             <|> (isSpace <$ string "space")
             <|> ((\c -> isAlphaNum c ||
                         generalCategory c == ConnectorPunctuation)
                   <$ string "word")
             <|> (isHexDigit <$ string "xdigit")
        _ <- string ":]"
        return $! if localNegated then not . res else res
  let getC = (char '\\' *> anyChar >>= pEscaped) <|>
             satisfy (\c -> c /= '\\' && c /= ']')
  let getCRange = do
        c <- getC
        (\d x -> x >= c && x <= d) <$> (char '-' *> getC) <|>
          return (== c)
  let getQELiteral = do
        void $ A.string "\\Q"
        cs <- manyTill anyChar (A.string "\\E")
        return $! \c -> any (== c) cs
  -- a ] in first position is a literal; it may also be the start of
  -- a range, as in []-a]:
  brack <- option [] $ do
    _ <- char ']'
    (do d <- char '-' *> getC
        return [\x -> x >= ']' && x <= d])
      <|> return [(== ']')]
  fs <- many (getQELiteral <|> getEscapedClass <|> getPosixClass <|> getCRange
              <|> (A.string "\\p" *> pUnicodeCharClass))
  void $ char ']'
  let f c = any ($ c) $ brack ++ fs
  -- for case-insensitive matching, a character matches (or, if
  -- negated, is excluded) if any of its case variants matches:
  let f' c | caseSensitive = f c
           | otherwise = f c || f (toLower c) || f (toUpper c)
  return $! MatchChar $ if negated
                           then not . f'
                           else f'

-- character class \p{Lo}; we assume \p is already parsed
pUnicodeCharClass :: Parser (Char -> Bool)
pUnicodeCharClass = do
  ds <- char '{' *> A.takeWhile (/= '}') <* char '}'
  return $
    (case ds of
      "Lu" -> (== UppercaseLetter)
      "Ll" -> (== LowercaseLetter)
      "Lt" -> (== TitlecaseLetter)
      "Lm" -> (== ModifierLetter)
      "Lo" -> (== OtherLetter)
      "L" -> (\c -> c == UppercaseLetter || c == LowercaseLetter ||
                    c == TitlecaseLetter || c == ModifierLetter ||
                    c == OtherLetter)
      "Mn" -> (== NonSpacingMark)
      "Mc" -> (== SpacingCombiningMark)
      "Me" -> (== EnclosingMark)
      "M" -> (\c -> c == NonSpacingMark || c == SpacingCombiningMark ||
                    c == EnclosingMark)
      "Nd" -> (== DecimalNumber)
      "Nl" -> (== LetterNumber)
      "No" -> (== OtherNumber)
      "N" -> (\c -> c == DecimalNumber || c == LetterNumber ||
                    c == OtherNumber)
      "Pc" -> (== ConnectorPunctuation)
      "Pd" -> (== DashPunctuation)
      "Ps" -> (== OpenPunctuation)
      "Pe" -> (== ClosePunctuation)
      "Pi" -> (== InitialQuote)
      "Pf" -> (== FinalQuote)
      "Po" -> (== OtherPunctuation)
      "P" -> (\c -> c == ConnectorPunctuation || c == DashPunctuation ||
                    c == OpenPunctuation || c == ClosePunctuation ||
                    c == InitialQuote || c == FinalQuote ||
                    c == OtherPunctuation)
      "Sm" -> (== MathSymbol)
      "Sc" -> (== CurrencySymbol)
      "Sk" -> (== ModifierSymbol)
      "So" -> (== OtherSymbol)
      "S" -> (\c -> c == MathSymbol || c == CurrencySymbol ||
                    c == ModifierSymbol || c == OtherSymbol)
      "Zs" -> (== Space)
      "Zl" -> (== LineSeparator)
      "Zp" -> (== ParagraphSeparator)
      "Z" -> (\c -> c == Space || c == LineSeparator ||
                    c == ParagraphSeparator)
      "Cc" -> (== Control)
      "Cf" -> (== Format)
      "Cs" -> (== Surrogate)
      "Co" -> (== PrivateUse)
      "Cn" -> (== NotAssigned)
      "C" -> (\c -> c == Control || c == Format || c == Surrogate ||
                    c == PrivateUse || c == NotAssigned)
      _    -> const False) . generalCategory


-- PCRE's \h matches this fixed list of horizontal whitespace
-- characters (which is not the same as Unicode category Zs):
isHorizSpace :: Char -> Bool
isHorizSpace c =
  c == '\t' || c == ' ' || c == '\xA0' || c == '\x1680' || c == '\x180E' ||
  (c >= '\x2000' && c <= '\x200A') || c == '\x202F' || c == '\x205F' ||
  c == '\x3000'

isSpecial :: Char -> Bool
isSpecial '\\' = True
isSpecial '?'  = True
isSpecial '*'  = True
isSpecial '+'  = True
-- isSpecial '{' = True -- this is okay except in suffixes
isSpecial '[' = True
-- an unmatched ] is treated as a literal (as in PCRE), so it is
-- not included here; the ] terminating a character class is consumed
-- by pRegexCharClass:
isSpecial '%' = True
isSpecial '(' = True
isSpecial ')' = True
isSpecial '|' = True
isSpecial '.' = True
isSpecial '$' = True
isSpecial '^' = True
isSpecial _  = False

