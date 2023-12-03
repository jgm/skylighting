{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Regex.KDE.Compile
  (compileRegex)
  where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
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
                       (decodeUtf8Lenient bs)
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
  foldr MatchAlt
    <$> pAltPart
    <*> many (lift (char '|') *> (pAltPart <|> pure mempty))

pAltPart :: RParser Regex
pAltPart = mconcat <$> many1 pRegexPart

pRegexPart :: RParser Regex
pRegexPart =
  pRegexChar <|> pParenthesized >>= pSuffix

pParenthesized :: RParser Regex
pParenthesized = do
  _ <- lift (char '(')
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
  contents <- option MatchNull $ withStateT stModifier $
    foldr MatchAlt
      <$> pAltPart
      <*> many (lift (char '|') *>
            ((when resetCaptureNumbers
                  (modify (\st ->
                        st{ rsCurrentCaptureNumber = currentCaptureNumber }))
               >> pAltPart) <|> pure mempty))
  _ <- lift (char ')')
  return $ modifier contents

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
     do c <- digit
        return (\_ -> Subroutine (ord c - 48), id)
   <|>
     do void $ char 'R'
        return  (\_ -> Subroutine 0, id)

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
  (case w of
    '*'  -> return $ MatchAlt (MatchSome re) MatchNull
    '+'  -> return $ MatchSome re
    '?'  -> return $ MatchAlt re MatchNull
    '{'  -> do
      minn <- lift $
        option Nothing $ readMay . T.unpack <$> A.takeWhile isDigit
      maxn <- lift $ option minn $ char ',' *>
                       (readMay . T.unpack <$> A.takeWhile isDigit)
      _ <- lift $ char '}'
      case (minn, maxn) of
          (Nothing, Nothing) -> mzero
          (Just n, Nothing)  -> return $! atleast n re
          (Nothing, Just n)  -> return $! atmost n re
          (Just m, Just n)   -> return $! between m n re
    _   -> fail "pSuffix encountered impossible byte") >>=
             lift . pQuantifierModifier
 where
   atmost 0 _ = MatchNull
   atmost n r = MatchAlt (mconcat (replicate n r)) (atmost (n-1) r)

   between 0 n r = atmost n r
   between m n r = mconcat (replicate m r) <> atmost (n - m) r

   atleast n r = mconcat (replicate n r) <> MatchAlt (MatchSome r) MatchNull

pQuantifierModifier :: Regex -> Parser Regex
pQuantifierModifier re = option re $
  (Possessive re <$ char '+') <|> (Lazy re <$ char '?')

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
    '\\' -> lift pRegexEscapedChar
    '$'  -> return AssertEnd
    '^'  -> return AssertBeginning
    '['  -> lift pRegexCharClass
    _ | isSpecial w -> mzero
      | otherwise -> return $!
            MatchChar $ if caseSensitive
                           then (== w)
                           else (\d -> toLower d == toLower w)

pRegexEscapedChar :: Parser Regex
pRegexEscapedChar = do
  c <- A.anyChar
  (case c of
    'b' -> return AssertWordBoundary
    'B' -> return $ AssertNegative Forward AssertWordBoundary
    '{' -> do -- captured pattern: \1 \2 \{12}
              ds <- many1 digit
              _ <- char '}'
              case readMay ds of
                Just !n -> return $ MatchCaptured n
                Nothing -> fail "not a number"
    'd' -> return $ MatchChar isDigit
    'D' -> return $ MatchChar (not . isDigit)
    's' -> return $ MatchChar isSpace
    'S' -> return $ MatchChar (not . isSpace)
    'w' -> return $ MatchChar isWordChar
    'W' -> return $ MatchChar (not . isWordChar)
    'p' -> MatchChar <$> pUnicodeCharClass
    _ | isDigit c ->
       return $! MatchCaptured (ord c - ord '0')
      | otherwise -> mzero) <|> (MatchChar . (==) <$> pEscaped c)

pEscaped :: Char -> Parser Char
pEscaped c =
  case c of
    '\\' -> return c
    'a' -> return '\a'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'v' -> return '\v'
    '0' -> do -- \0ooo matches octal ooo
      ds <- A.take 3
      case readMay ("'\\o" ++ T.unpack ds ++ "'") of
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

pRegexCharClass :: Parser Regex
pRegexCharClass = do
  negated <- option False $ True <$ char '^'
  let getEscapedClass = do
        _ <- char '\\'
        (isDigit <$ char 'd')
         <|> (not . isDigit <$ char 'D')
         <|> (isSpace <$ char 's')
         <|> (not . isSpace <$ char 'S')
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
             <|> ((\c -> isPrint c || isSpace c) <$ string "graph:")
             <|> (isLower <$ string "lower")
             <|> (isUpper <$ string "upper")
             <|> (isPrint <$ string "print")
             <|> (isPunctuation <$ string "punct")
             <|> (isSpace <$ string "space")
             <|> ((\c -> isAlphaNum c ||
                         generalCategory c == ConnectorPunctuation)
                   <$ string "word:")
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
  brack <- option [] $ [(==']')] <$ char ']'
  fs <- many (getQELiteral <|> getEscapedClass <|> getPosixClass <|> getCRange
              <|> (A.string "\\p" *> pUnicodeCharClass))
  void $ char ']'
  let f c = any ($ c) $ brack ++ fs
  return $! MatchChar $ if negated
                           then not . f
                           else f

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


isSpecial :: Char -> Bool
isSpecial '\\' = True
isSpecial '?'  = True
isSpecial '*'  = True
isSpecial '+'  = True
-- isSpecial '{' = True -- this is okay except in suffixes
isSpecial '[' = True
isSpecial ']' = True
isSpecial '%' = True
isSpecial '(' = True
isSpecial ')' = True
isSpecial '|' = True
isSpecial '.' = True
isSpecial '$' = True
isSpecial '^' = True
isSpecial _  = False

