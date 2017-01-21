{-# LANGUAGE OverloadedStrings #-}
module Skylighting.Tokenizer (
    tokenize
  , TokenizerConfig(..)
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (mk)
import Data.Char (isAlphaNum, isLetter, isSpace, ord)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.ByteString.UTF8 as UTF8
import Debug.Trace
import Skylighting.Regex
import Skylighting.Types

info :: String -> TokenizerM ()
info s = do
  tr <- asks traceOutput
  when tr $ trace s (return ())

infoContextStack :: TokenizerM ()
infoContextStack = do
  tr <- asks traceOutput
  when tr $ do
    ContextStack stack <- gets contextStack
    info $ "CONTEXT STACK " ++ show (map cName stack)

newtype ContextStack = ContextStack{ unContextStack :: [Context] }
  deriving (Show)

data TokenizerState = TokenizerState{
    input               :: BS.ByteString
  , prevChar            :: Char
  , contextStack        :: ContextStack
  , captures            :: [BS.ByteString]
  , column              :: Int
  , lineContinuation    :: Bool
  , firstNonspaceColumn :: Maybe Int
} deriving (Show)

-- | Configuration options for 'tokenize'.
data TokenizerConfig = TokenizerConfig{
    syntaxMap   :: SyntaxMap  -- ^ Syntax map to use
  , traceOutput :: Bool       -- ^ Generate trace output for debugging
} deriving (Show)

type TokenizerM =
  ExceptT String (ReaderT TokenizerConfig (State TokenizerState))

popContextStack :: TokenizerM ()
popContextStack = do
  ContextStack cs <- gets contextStack
  case cs of
       []     -> throwError "Empty context stack" -- programming error
       (_:[]) -> return ()  -- don't pop last context
       (_:rest) -> do
         modify (\st -> st{ contextStack = ContextStack rest })
         infoContextStack

pushContextStack :: Context -> TokenizerM ()
pushContextStack cont = do
  modify (\st -> st{ contextStack =
                      ContextStack (cont : unContextStack (contextStack st)) } )
  infoContextStack

currentContext :: TokenizerM Context
currentContext = do
  ContextStack cs <- gets contextStack
  case cs of
       []    -> throwError "Empty context stack" -- programming error
       (c:_) -> return c

doContextSwitch :: [ContextSwitch] -> TokenizerM ()
doContextSwitch [] = return ()
doContextSwitch (Pop : xs) = do
  popContextStack
  currentContext >>= checkLineEnd
  doContextSwitch xs
doContextSwitch (Push (syn,c) : xs) = do
  syntaxes <- asks syntaxMap
  case Map.lookup syn syntaxes >>= lookupContext c of
       Just con -> do
         pushContextStack con
         checkLineEnd con
         doContextSwitch xs
       Nothing  -> throwError $ "Unknown syntax or context: " ++ show (syn, c)

lookupContext :: Text -> Syntax -> Maybe Context
lookupContext name syntax | Text.null name =
  if Text.null (sStartingContext syntax)
     then Nothing
     else lookupContext (sStartingContext syntax) syntax
lookupContext name syntax = Map.lookup name $ sContexts syntax

-- | Tokenize some text using 'Syntax'.
tokenize :: TokenizerConfig -> Syntax -> Text -> Either String [SourceLine]
tokenize config syntax inp =
  evalState
    (runReaderT
      (runExceptT (mapM tokenizeLine $
        zip (BS.lines $ encodeUtf8 inp) [1..])) config)
    startingState{ input = encodeUtf8 inp
                 , contextStack = case lookupContext
                                       (sStartingContext syntax) syntax of
                                       Just c  -> ContextStack [c]
                                       Nothing -> ContextStack [] }

startingState :: TokenizerState
startingState =
  TokenizerState{ input = BS.empty
                , prevChar = '\n'
                , contextStack = ContextStack []
                , captures = []
                , column = 0
                , lineContinuation = False
                , firstNonspaceColumn = Nothing
                }

tokenizeLine :: (BS.ByteString, Int) -> TokenizerM [Token]
tokenizeLine (ln, linenum) = do
  cur <- currentContext
  lineCont <- gets lineContinuation
  if lineCont
     then modify $ \st -> st{ lineContinuation = False }
     else do
       modify $ \st -> st{ column = 0
                         , firstNonspaceColumn =
                              BS.findIndex (not . isSpace) ln }
       doContextSwitch (cLineBeginContext cur)
  if BS.null ln
     then doContextSwitch (cLineEmptyContext cur)
     else doContextSwitch (cLineBeginContext cur)
  modify $ \st -> st{ input = ln, prevChar = '\n' }
  ts <- normalizeHighlighting . catMaybes <$> many getToken
  currentContext >>= checkLineEnd
  -- fail if we haven't consumed whole line
  inp <- gets input
  if not (BS.null inp)
     then do
       col <- gets column
       throwError $ "Could not match anything at line " ++
         show linenum ++ " column " ++ show col
     else return ts

getToken :: TokenizerM (Maybe Token)
getToken = do
  inp <- gets input
  guard $ not (BS.null inp)
  context <- currentContext
  msum (map tryRule (cRules context)) <|>
     if cFallthrough context
        then doContextSwitch (cFallthroughContext context) >> getToken
        else (\x -> Just (cAttribute context, x)) <$> normalChunk

takeChars :: Int -> TokenizerM Text
takeChars 0 = mzero
takeChars numchars = do
  inp <- gets input
  let (bs,rest) = UTF8.splitAt numchars inp
  guard $ not (BS.null bs)
  t <- decodeBS bs
  modify $ \st -> st{ input = rest,
                      prevChar = Text.last t,
                      column = column st + numchars }
  return t

tryRule :: Rule -> TokenizerM (Maybe Token)
tryRule rule = do
  case rColumn rule of
       Nothing -> return ()
       Just n  -> do
         col <- gets column
         guard (col == n)

  when (rFirstNonspace rule) $ do
    firstNonspace <- gets firstNonspaceColumn
    col <- gets column
    guard (firstNonspace == Just col)

  oldstate <- if rLookahead rule
                 then Just <$> get -- needed for lookahead rules
                 else return Nothing

  let attr = rAttribute rule
  mbtok <- case rMatcher rule of
                DetectChar c -> withAttr attr $ detectChar (rDynamic rule) c
                Detect2Chars c d -> withAttr attr $
                                      detect2Chars (rDynamic rule) c d
                AnyChar cs -> withAttr attr $ anyChar cs
                RangeDetect c d -> withAttr attr $ rangeDetect c d
                RegExpr re -> withAttr attr $ regExpr (rDynamic rule) re
                Int -> withAttr attr $ regExpr False integerRegex
                HlCOct -> withAttr attr $ regExpr False octRegex
                HlCHex -> withAttr attr $ regExpr False hexRegex
                HlCStringChar -> withAttr attr $
                                     regExpr False hlCStringCharRegex
                HlCChar -> withAttr attr $ regExpr False hlCCharRegex
                Float -> withAttr attr $ regExpr False floatRegex
                Keyword kwattr kws ->
                  withAttr attr $ keyword kwattr kws
                StringDetect s -> withAttr attr $
                                    stringDetect (rCaseSensitive rule) s
                WordDetect s -> withAttr attr $
                                    wordDetect (rCaseSensitive rule) s
                LineContinue -> withAttr attr $ lineContinue
                DetectSpaces -> withAttr attr $ detectSpaces
                DetectIdentifier -> withAttr attr $ detectIdentifier
                IncludeRules cname -> includeRules
                   (if rIncludeAttribute rule then Just attr else Nothing)
                   cname
  mbchildren <- msum (map tryRule (rChildren rule))
                 <|> return Nothing

  mbtok' <- case mbtok of
                 Nothing -> return Nothing
                 Just (tt, s)
                   | rLookahead rule -> do
                     (oldinput, oldprevChar, oldColumn) <-
                         case oldstate of
                              Nothing -> throwError
                                    "oldstate not saved with lookahead rule"
                              Just st -> return
                                    (input st, prevChar st, column st)
                     modify $ \st -> st{ input = oldinput
                                       , prevChar = oldprevChar
                                       , column = oldColumn }
                     return Nothing
                   | otherwise -> do
                     case mbchildren of
                          Nothing -> return $ Just (tt, s)
                          Just (_, cresult) -> return $ Just (tt, s <> cresult)

  info $ takeWhile (/=' ') (show (rMatcher rule)) ++ " MATCHED " ++ show mbtok'
  doContextSwitch (rContextSwitch rule)
  return mbtok'

withAttr :: TokenType -> TokenizerM Text -> TokenizerM (Maybe Token)
withAttr tt p = do
  res <- p
  case res of
       "" -> return Nothing
       xs -> return $ Just (tt, xs)

hlCStringCharRegex :: RE
hlCStringCharRegex = RE{
    reString = reHlCStringChar
  , reCompiled = Just $ compileRegex False reHlCStringChar
  , reCaseSensitive = False
  }

reHlCStringChar :: BS.ByteString
reHlCStringChar = "\\\\(?:[abefnrtv\"'?\\\\]|[xX][a-fA-F0-9]+|0[0-7]+)"

hlCCharRegex :: RE
hlCCharRegex = RE{
    reString = reStr
  , reCompiled = Just $ compileRegex False reStr
  , reCaseSensitive = False
  }
  where reStr = "'(?:" <> reHlCStringChar <> "|[^'\\\\])'"

wordDetect :: Bool -> Text -> TokenizerM Text
wordDetect caseSensitive s = do
  res <- stringDetect caseSensitive s
  -- now check for word boundary:  (TODO: check to make sure this is correct)
  inp <- gets input
  case UTF8.uncons inp of
       Just (c, _) | not (isAlphaNum c) -> return res
       _                                -> mzero

stringDetect :: Bool -> Text -> TokenizerM Text
stringDetect caseSensitive s = do
  inp <- gets input
  let s' = encodeUtf8 s
  let len = BS.length s'
  let t' = BS.take len inp
  t <- decodeBS t'
  -- we assume here that the case fold will not change length,
  -- which is safe for ASCII keywords and the like...
  let matches = if caseSensitive
                   then s == t
                   else mk s == mk t
  if matches
     then takeChars (Text.length s)
     else mzero

-- This assumes that nothing significant will happen
-- in the middle of a string of spaces or a string
-- of alphanumerics.  This seems true  for all normal
-- programming languages, and the optimization speeds
-- things up a lot, relative to just parsing one char.
normalChunk :: TokenizerM Text
normalChunk = do
  inp <- gets input
  case BS.uncons inp of
    Nothing -> mzero
    Just (c, _)
      | c == ' ' ->
        let (bs,_) = BS.span (==' ') inp
        in  takeChars (BS.length bs)
      | isAlphaNum c ->
        let (bs,_) = BS.span isAlphaNum inp
        in  takeChars (BS.length bs)
      | otherwise -> takeChars 1

includeRules :: Maybe TokenType -> ContextName -> TokenizerM (Maybe Token)
includeRules mbattr (syn, con) = do
  syntaxes <- asks syntaxMap
  case Map.lookup syn syntaxes >>= lookupContext con of
       Nothing  -> throwError $ "Context lookup failed " ++ show (syn, con)
       Just c   -> do
         mbtok <- msum (map tryRule (cRules c))
         checkLineEnd c
         return $ case (mbtok, mbattr) of
                    (Just (NormalTok, xs), Just attr) ->
                       Just (attr, xs)
                    _ -> mbtok

checkLineEnd :: Context -> TokenizerM ()
checkLineEnd c = do
  inp <- gets input
  when (BS.null inp) $ do
    lineCont' <- gets lineContinuation
    unless lineCont' $ doContextSwitch (cLineEndContext c)

detectChar :: Bool -> Char -> TokenizerM Text
detectChar dynamic c = do
  c' <- if dynamic && c >= '0' && c <= '9'
           then getDynamicChar c
           else return c
  inp <- gets input
  case UTF8.uncons inp of
    Just (x,_) | x == c' -> takeChars 1
    _          -> mzero

getDynamicChar :: Char -> TokenizerM Char
getDynamicChar c = do
  let capNum = ord c - ord '0'
  res <- getCapture capNum
  case Text.uncons res of
       Nothing    -> mzero
       Just (d,_) -> return d

detect2Chars :: Bool -> Char -> Char -> TokenizerM Text
detect2Chars dynamic c d = do
  c' <- if dynamic && c >= '0' && c <= '9'
           then getDynamicChar c
           else return c
  d' <- if dynamic && d >= '0' && d <= '9'
           then getDynamicChar d
           else return d
  inp <- gets input
  if (encodeUtf8 (Text.pack [c',d'])) `BS.isPrefixOf` inp
     then takeChars 2
     else mzero

-- NOTE: currently this will only work for ASCII open/close.
rangeDetect :: Char -> Char -> TokenizerM Text
rangeDetect c d = do
  inp <- gets input
  case BS.uncons inp of
    Just (x, rest)
      | x == c -> case BS.span (/= d) rest of
                       (in_t, out_t)
                         | BS.null out_t -> mzero
                         | otherwise -> do
                              t <- decodeBS in_t
                              takeChars (Text.length t + 2)
    _ -> mzero

-- NOTE: currently limited to ASCII
detectSpaces :: TokenizerM Text
detectSpaces = do
  inp <- gets input
  case BS.span (\c -> isSpace c) inp of
       (t, _)
         | BS.null t -> mzero
         | otherwise -> takeChars (BS.length t)

-- NOTE: limited to ASCII
detectIdentifier :: TokenizerM Text
detectIdentifier = do
  inp <- gets input
  case BS.uncons inp of
    Just (c, t) | isLetter c || c == '_' ->
      takeChars $ 1 + maybe 0 id (BS.findIndex
                (\d -> not (isAlphaNum d || d == '_')) t)
    _ -> mzero

lineContinue :: TokenizerM Text
lineContinue = do
  inp <- gets input
  if inp == "\\"
     then do
       modify $ \st -> st{ lineContinuation = True }
       takeChars 1
     else mzero

anyChar :: [Char] -> TokenizerM Text
anyChar cs = do
  inp <- gets input
  case UTF8.uncons inp of
     Just (x, _) | x `elem` cs -> takeChars 1
     _           -> mzero

regExpr :: Bool -> RE -> TokenizerM Text
regExpr dynamic re = do
  reStr <- if dynamic
              then subDynamic (reString re)
              else return (reString re)
  -- note, for dynamic regexes rCompiled == Nothing:
  let regex = fromMaybe (compileRegex (reCaseSensitive re) reStr)
                 $ reCompiled re
  inp <- gets input
  -- If regex starts with \b, determine if we're at a word
  -- boundary and mzero if not (TODO - is this the correct
  -- definition of a word boundary?)
  when (BS.take 2 reStr == "\\b") $
       case UTF8.uncons inp of
            Nothing -> return ()
            Just (c, _) -> do
              prev <- gets prevChar
              if isAlphaNum prev
                 then guard (not (isAlphaNum c))
                 else guard (isAlphaNum c)
  case matchRegex regex inp of
       Just (match:capts) -> do
         match' <- decodeBS match
         modify $ \st -> st{ captures = capts }
         takeChars (Text.length match')
       _ -> mzero

decodeBS :: BS.ByteString -> TokenizerM Text
decodeBS bs = case decodeUtf8' bs of
                    Left _ -> throwError ("ByteString " ++
                                show bs ++ "is not UTF8")
                    Right t -> return t

-- Substitute out %1, %2, etc. in regex string, escaping
-- appropriately..
subDynamic :: BS.ByteString -> TokenizerM BS.ByteString
subDynamic bs
  | BS.null bs = return BS.empty
  | otherwise  =
    case BS.unpack (BS.take 2 bs) of
        ['%',x] | x >= '0' && x <= '9' -> do
           let capNum = ord x - ord '0'
           let escapeRegexChar c
                | c `elem` ['^','$','\\','[',']','(',')','{','}','*','+','.','?']
                            = BS.pack ['\\',c]
                | otherwise = BS.singleton c
           let escapeRegex = BS.concatMap escapeRegexChar
           replacement <- getCapture capNum
           (escapeRegex (encodeUtf8 replacement) <>) <$> subDynamic (BS.drop 2 bs)
        _ -> case BS.break (=='%') bs of
                  (y,z)
                    | BS.null y -> BS.cons '%' <$> subDynamic z
                    | BS.null z -> return y
                    | otherwise -> (y <>) <$> subDynamic z

getCapture :: Int -> TokenizerM Text
getCapture capnum = do
  capts <- gets captures
  if length capts < capnum
     then mzero
     else decodeBS $ capts !! (capnum - 1)

-- TODO this assumes that delims are ascii; check in the parser?
keyword :: KeywordAttr -> WordSet Text -> TokenizerM Text
keyword kwattr kws = do
  prev <- gets prevChar
  guard $ prev `Set.member` (keywordDelims kwattr)
  inp <- gets input
  let (w,_) = BS.break (`Set.member` (keywordDelims kwattr)) inp
  guard $ not (BS.null w)
  w' <- decodeBS w
  let numchars = Text.length w'
  case kws of
       CaseSensitiveWords ws   | w' `Set.member` ws -> takeChars numchars
       CaseInsensitiveWords ws | mk w' `Set.member` ws -> takeChars numchars
       _                       -> mzero

normalizeHighlighting :: [Token] -> [Token]
normalizeHighlighting [] = []
normalizeHighlighting ((t,x):xs)
  | Text.null x = normalizeHighlighting xs
  | otherwise =
    (t, Text.concat (x : map snd matches)) : normalizeHighlighting rest
    where (matches, rest) = span (\(z,_) -> z == t) xs

integerRegex :: RE
integerRegex = RE{
    reString = intReStr
  , reCompiled = Just $ compileRegex False intReStr
  , reCaseSensitive = False
  }
  where intReStr = "\\b[-+]?(0[Xx][0-9A-Fa-f]+|0[Oo][0-7]+|[0-9]+)\\b"

floatRegex :: RE
floatRegex = RE{
    reString = floatReStr
  , reCompiled = Just $ compileRegex False floatReStr
  , reCaseSensitive = False
  }
  where floatReStr = "\\b[-+]?(([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+)\\b"

octRegex :: RE
octRegex = RE{
    reString = octRegexStr
  , reCompiled = Just $ compileRegex False octRegexStr
  , reCaseSensitive = False
  }
  where octRegexStr = "\\b[-+]?0[Oo][0-7]+\\b"

hexRegex :: RE
hexRegex = RE{
    reString = hexRegexStr
  , reCompiled = Just $ compileRegex False hexRegexStr
  , reCaseSensitive = False
  }
  where hexRegexStr = "\\b[-+]?0[Xx][0-9A-Fa-f]+\\b"

