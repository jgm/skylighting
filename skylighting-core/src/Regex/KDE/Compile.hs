{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Regex.KDE.Compile
  (compileRegex)
  where

import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as U
import Safe
import Data.Attoparsec.ByteString as A hiding (match)
import Data.Char
import Control.Applicative
import Regex.KDE.Regex
import Control.Monad.State.Strict
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

-- | Compile a UTF-8 encoded ByteString as a Regex.  If the first
-- parameter is True, then the Regex will be case sensitive.
compileRegex :: Bool -> ByteString -> Either String Regex
compileRegex caseSensitive bs =
  let !res = parseOnly (evalStateT parser 0) bs
   in res
 where
   parser = do
     !re <- pRegex caseSensitive
     (re <$ lift A.endOfInput) <|>
       do rest <- lift A.takeByteString
          fail $ "parse error at byte position " ++
                 show (B.length bs - B.length rest)

type RParser = StateT Int Parser

pRegex :: Bool -> RParser Regex
pRegex caseSensitive =
  option MatchNull $
  foldr MatchAlt
    <$> (pAltPart caseSensitive)
    <*> (many $ lift (char '|') *> (pAltPart caseSensitive <|> pure mempty))

pAltPart :: Bool -> RParser Regex
pAltPart caseSensitive = mconcat <$> many1 (pRegexPart caseSensitive)

char :: Char -> Parser Char
char c =
  c <$ satisfy (== fromIntegral (ord c))

pRegexPart :: Bool -> RParser Regex
pRegexPart caseSensitive =
  (lift (pRegexChar caseSensitive) <|> pParenthesized caseSensitive) >>=
     lift . pSuffix

pParenthesized :: Bool -> RParser Regex
pParenthesized caseSensitive = do
  _ <- lift (satisfy (== 40))
  modifier <- lift (satisfy (== 63) *> pGroupModifiers)
                <|> (MatchCapture <$> (modify (+ 1) *> get))
  contents <- pRegex caseSensitive
  _ <- lift (satisfy (== 41))
  return $ modifier contents

pGroupModifiers :: Parser (Regex -> Regex)
pGroupModifiers =
  (id <$ char ':')
   <|>
     do dir <- option Forward $ Backward <$ char '<'
        (AssertPositive dir <$ char '=') <|> (AssertNegative dir <$ char '!')

pSuffix :: Regex -> Parser Regex
pSuffix re = option re $ do
  w <- satisfy (\x -> x == 42 || x == 43 || x == 63 || x == 123)
  (case w of
    42  -> return $ MatchAlt (MatchSome re) MatchNull
    43  -> return $ MatchSome re
    63  -> return $ MatchAlt re MatchNull
    123 -> do
      let isDig x = x >= 48 && x < 58
      minn <- option Nothing $ readMay . U.toString <$> A.takeWhile isDig
      maxn <- option Nothing $ char ',' *>
                       (readMay . U.toString <$> A.takeWhile isDig)
      _ <- char '}'
      return $!
        case (minn, maxn) of
          (Nothing, Nothing) -> atleast 0 re
          (Just n, Nothing)  -> atleast n re
          (Nothing, Just n)  -> atmost n re
          (Just m, Just n)   -> between m n re
    _   -> fail "pSuffix encountered impossible byte") >>= pSuffix
 where
   atmost 0 _ = MatchNull
   atmost n r = MatchAlt (mconcat (replicate n r)) (atmost (n-1) r)

   between 0 n r = atmost n r
   between m n r = mconcat (replicate m r) <> atmost (n - m) r

   atleast n r = mconcat (replicate n r) <> MatchAlt (MatchSome r) MatchNull

pRegexChar :: Bool -> Parser Regex
pRegexChar caseSensitive = do
  w <- satisfy $ const True
  case w of
    46  -> return MatchAnyChar
    37 -> (do -- dynamic %1 %2
              ds <- A.takeWhile1 (\x -> x >= 48 && x <= 57)
              case readMay (U.toString ds) of
                Just !n -> return $ MatchDynamic n
                Nothing -> fail "not a number")
            <|> return (MatchChar (== '%'))
    92  -> pRegexEscapedChar
    36  -> return AssertEnd
    94  -> return AssertBeginning
    91  -> pRegexCharClass
    _ | w < 128
      , not (isSpecial w)
         -> do let c = chr $ fromIntegral w
               return $! MatchChar $
                        if caseSensitive
                           then (== c)
                           else (\d -> toLower d == toLower c)
      | w >= 0xc0 -> do
          rest <- case w of
                    _ | w >= 0xf0 -> A.take 3
                      | w >= 0xe0 -> A.take 2
                      | otherwise -> A.take 1
          case U.uncons (B.cons w rest) of
            Just (d, _) -> return $! MatchChar $
                             if caseSensitive
                                then (== d)
                                else (\e -> toLower e == toLower d)
            Nothing     -> fail "could not decode as UTF8"
      | otherwise -> mzero

pRegexEscapedChar :: Parser Regex
pRegexEscapedChar = do
  c <- anyChar
  (case c of
    'b' -> return AssertWordBoundary
    '{' -> do -- captured pattern: \1 \2 \{12}
              ds <- A.takeWhile1 (\x -> x >= 48 && x <= 57)
              _ <- char '}'
              case readMay (U.toString ds) of
                Just !n -> return $ MatchCaptured $ n
                Nothing -> fail "not a number"
    'd' -> return $ MatchChar isDigit
    'D' -> return $ MatchChar (not . isDigit)
    's' -> return $ MatchChar isSpace
    'S' -> return $ MatchChar (not . isSpace)
    'w' -> return $ MatchChar isWordChar
    'W' -> return $ MatchChar (not . isWordChar)
    _ | c >= '0' && c <= '9' ->
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
      case readMay ("'\\o" ++ U.toString ds ++ "'") of
        Just x  -> return x
        Nothing -> fail "invalid octal character escape"
    'z' -> do -- \zhhhh matches unicode hex char hhhh
      ds <- A.take 4
      case readMay ("'\\x" ++ U.toString ds ++ "'") of
        Just x  -> return x
        Nothing -> fail "invalid hex character escape"
    _ | c >= '1' && c <= '7' -> do -- \ooo octal undocument form but works
         ds <- A.take 2
         case readMay ("'\\o" ++ c : U.toString ds ++ "'") of
           Just x  -> return x
           Nothing -> fail "invalid octal character escape"
      | otherwise -> return c

pRegexCharClass :: Parser Regex
pRegexCharClass = do
  negated <- option False $ True <$ satisfy (== 94) -- '^'
  let getEscapedClass = do
        _ <- satisfy (== 92) -- backslash
        (isDigit <$ char 'd')
         <|> (not . isDigit <$ char 'D')
         <|> (isSpace <$ char 's')
         <|> (not . isSpace <$ char 'S')
         <|> (isWordChar <$ char 'w')
         <|> (not . isWordChar <$ char 'W')
  let getPosixClass = do
        _ <- string "[:"
        localNegated <- option False $ True <$ satisfy (== 94) -- '^'
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
  let getC = (satisfy (== 92) *> anyChar >>= pEscaped) <|>
       (chr . fromIntegral <$> satisfy (\x -> x /= 92 && x /= 93)) -- \ ]
  let getCRange = do
        c <- getC
        (\d -> (\x -> x >= c && x <= d)) <$> (char '-' *> getC) <|>
          return (== c)
  brack <- option [] $ [(==']')] <$ char ']'
  fs <- many (getEscapedClass <|> getPosixClass <|> getCRange)
  _ <- satisfy (== 93) -- ]
  let f c = any ($ c) $ brack ++ fs
  return $! MatchChar (if negated then (not . f) else f)

anyChar :: Parser Char
anyChar = do
  w <- satisfy (const True)
  return $! chr $ fromIntegral w

isSpecial :: Word8 -> Bool
isSpecial 92 = True -- '\\'
isSpecial 63 = True -- '?'
isSpecial 42 = True -- '*'
isSpecial 43 = True -- '+'
isSpecial 123 = True -- '{'
isSpecial 91 = True -- '['
isSpecial 93 = True -- ']'
isSpecial 37 = True -- '%'
isSpecial 40 = True -- '('
isSpecial 41 = True -- ')'
isSpecial 124 = True -- '|'
isSpecial 46 = True -- '.'
isSpecial 36 = True -- '$'
isSpecial 94 = True -- '^'
isSpecial _  = False

