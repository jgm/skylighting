{-# LANGUAGE TupleSections #-}
module Skylighting.Tokenizer (
    tokenize
  , TokenizerConfig(..)
  ) where

import qualified Data.Set as Set
import Skylighting.Regex
import Skylighting.Types
import Data.List (findIndex)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Char (isSpace, isLetter, isAlphaNum, ord)
import qualified Data.Map as Map
import Debug.Trace
import Data.CaseInsensitive (mk)
import Data.Maybe (catMaybes)

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
    input        :: String
  , prevChar     :: Char
  , contextStack :: ContextStack
  , captures     :: [String]
  , column       :: Int
  , lineContinuation :: Bool
  , firstNonspaceColumn :: Maybe Int
} deriving (Show)

data TokenizerConfig = TokenizerConfig{
    syntaxMap     :: SyntaxMap
  , traceOutput   :: Bool
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

lookupContext :: String -> Syntax -> Maybe Context
lookupContext "" syntax =
  case sStartingContext syntax of
       "" -> Nothing
       n  -> lookupContext n syntax
lookupContext name syntax = Map.lookup name $ sContexts syntax

tokenize :: TokenizerConfig -> Syntax -> String -> Either String [SourceLine]
tokenize config syntax inp =
  evalState
    (runReaderT
      (runExceptT (mapM tokenizeLine $ zip (lines inp) [1..])) config)
    startingState{ input = inp
                 , contextStack = case lookupContext
                                       (sStartingContext syntax) syntax of
                                       Just c -> ContextStack [c]
                                       Nothing -> ContextStack [] }

startingState :: TokenizerState
startingState =
  TokenizerState{ input = ""
                , prevChar = '\n'
                , contextStack = ContextStack []
                , captures = []
                , column = 0
                , lineContinuation = False
                , firstNonspaceColumn = Nothing
                }

tokenizeLine :: (String, Int) -> TokenizerM [Token]
tokenizeLine (ln, linenum) = do
  cur <- currentContext
  lineCont <- gets lineContinuation
  if lineCont
     then modify $ \st -> st{ lineContinuation = False }
     else do
       modify $ \st -> st{ column = 0
                         , firstNonspaceColumn =
                              findIndex (not . isSpace) ln }
       doContextSwitch (cLineBeginContext cur)
  if null ln
     then doContextSwitch (cLineEmptyContext cur)
     else doContextSwitch (cLineBeginContext cur)
  modify $ \st -> st{ input = ln, prevChar = '\n' }
  ts <- normalizeHighlighting . catMaybes <$> many getToken
  currentContext >>= checkLineEnd
  -- fail if we haven't consumed whole line
  inp <- gets input
  if not (null inp)
     then do
       col <- gets column
       throwError $ "Could not match anything at line " ++
         show linenum ++ " column " ++ show col
     else return ts

getToken :: TokenizerM (Maybe Token)
getToken = do
  inp <- gets input
  guard $ not (null inp)
  context <- currentContext
  msum (map tryRule (cRules context)) <|>
     if cFallthrough context
        then doContextSwitch (cFallthroughContext context) >> getToken
        else (\x -> Just (cAttribute context, x)) <$> normalChunk

takeChars :: String -> TokenizerM String
takeChars [] = mzero
takeChars xs = do
  let numchars = length xs
  modify $ \st -> st{ input = drop numchars (input st),
                      prevChar = last xs,
                      column = column st + numchars }
  return xs

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

  oldstate <- get -- needed for lookahead rules

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
                     modify $ \st -> st{ input = input oldstate
                                       , prevChar = prevChar oldstate
                                       , column = column oldstate }
                     return Nothing
                   | otherwise -> do
                     case mbchildren of
                          Nothing -> return $ Just (tt, s)
                          Just (_, cresult) -> return $ Just (tt, s ++ cresult)

  info $ takeWhile (/=' ') (show (rMatcher rule)) ++ " MATCHED " ++ show mbtok'
  doContextSwitch (rContextSwitch rule)
  return mbtok'

withAttr :: TokenType -> TokenizerM String -> TokenizerM (Maybe Token)
withAttr tt p = do
  res <- p
  case res of
       ""  -> return Nothing
       xs  -> return $ Just (tt, xs)

hlCStringCharRegex :: RE
hlCStringCharRegex = RE{
    reString = reHlCStringChar
  , reCompiled = Just $ compileRegex False reHlCStringChar
  , reCaseSensitive = False
  }

reHlCStringChar :: [Char]
reHlCStringChar = "\\\\(?:[abefnrtv\"'?\\\\]|[xX][a-fA-F0-9]+|0[0-7]+)"

hlCCharRegex :: RE
hlCCharRegex = RE{
    reString = reStr
  , reCompiled = Just $ compileRegex False reStr
  , reCaseSensitive = False
  }
  where reStr = "'(?:" ++ reHlCStringChar ++ "|[^'\\\\])'"

wordDetect :: Bool -> String -> TokenizerM String
wordDetect caseSensitive s = do
  res <- stringDetect caseSensitive s
  -- now check for word boundary:  (TODO: check to make sure this is correct)
  inp <- gets input
  case inp of
       (c:_) | not (isAlphaNum c) -> return res
       _ -> mzero

stringDetect :: Bool -> String -> TokenizerM String
stringDetect caseSensitive s = do
  inp <- gets input
  let t = take (length s) inp
  let matches = if caseSensitive
                   then s == t
                   else mk s == mk t
  if matches
     then takeChars s
     else mzero

-- This assumes that nothing significant will happen
-- in the middle of a string of spaces or a string
-- of alphanumerics.  This seems true  for all normal
-- programming languages, and the optimization speeds
-- things up a lot, relative to just parsing one char.
normalChunk :: TokenizerM String
normalChunk = do
  inp <- gets input
  case inp of
    [] -> mzero
    (' ':xs) -> takeChars $ ' ' : takeWhile (== ' ') xs
    (x:xs) | isAlphaNum x -> takeChars $ x : takeWhile isAlphaNum xs
    (x:_) -> takeChars [x]

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
  when (null inp) $ do
    lineCont' <- gets lineContinuation
    unless lineCont' $ doContextSwitch (cLineEndContext c)

detectChar :: Bool -> Char -> TokenizerM String
detectChar dynamic c = do
  c' <- if dynamic && c >= '0' && c <= '9'
           then getDynamicChar c
           else return c
  inp <- gets input
  case inp of
    (x:_) | x == c' -> takeChars [x]
    _ -> mzero

getDynamicChar :: Char -> TokenizerM Char
getDynamicChar c = do
  let capNum = ord c - ord '0'
  res <- getCapture capNum
  case res of
       []    -> mzero
       (d:_) -> return d

detect2Chars :: Bool -> Char -> Char -> TokenizerM String
detect2Chars dynamic c d = do
  c' <- if dynamic && c >= '0' && c <= '9'
           then getDynamicChar c
           else return c
  d' <- if dynamic && d >= '0' && d <= '9'
           then getDynamicChar d
           else return d
  inp <- gets input
  case inp of
    (x:y:_) | x == c' && y == d' -> takeChars [x,y]
    _ -> mzero

rangeDetect :: Char -> Char -> TokenizerM String
rangeDetect c d = do
  inp <- gets input
  case inp of
    (x:rest) | x == c -> case span (/= d) rest of
                              (xs, y:_) -> takeChars (x : xs ++ [y])
                              (_, [])   -> mzero
    _ -> mzero

detectSpaces :: TokenizerM String
detectSpaces = do
  inp <- gets input
  case span isSpace inp of
       ([], _) -> mzero
       (xs, _) -> takeChars xs

detectIdentifier :: TokenizerM String
detectIdentifier = do
  inp <- gets input
  case inp of
    (c:cs) | isLetter c || c == '_' ->
      takeChars $ c : takeWhile (\d -> isAlphaNum d || d == '_') cs
    _ -> mzero

lineContinue :: TokenizerM String
lineContinue = do
  inp <- gets input
  case inp of
     ['\\'] -> do
       modify $ \st -> st{ lineContinuation = True }
       takeChars "\\"
     _ -> mzero

--- TODO eventually make this a set of Char
anyChar :: [Char] -> TokenizerM String
anyChar cs = do
  inp <- gets input
  case inp of
     (x:_) | x `elem` cs -> takeChars [x]
     _ -> mzero

regExpr :: Bool -> RE -> TokenizerM String
regExpr dynamic re = do
  reStr <- if dynamic
              then subDynamic (reString re)
              else return (reString re)
  -- note, for dynamic regexes rCompiled == Nothing:
  regex <- maybe (return $ compileRegex (reCaseSensitive re) reStr)
                 return $ reCompiled re
  inp <- gets input
  prev <- gets prevChar
  -- we keep one preceding character, so initial \b can match:
  let target = if prev == '\n'
                  then ' ':inp
                  else prev:inp
  case matchRegex regex target of
       Just ((_:match):capts) -> do
         modify $ \st -> st{ captures = capts }
         takeChars match
       _ -> mzero

-- Substitute out %1, %2, etc. in regex string, escaping
-- appropriately..
subDynamic :: String -> TokenizerM String
subDynamic ('%':x:xs) | x >= '0' && x <= '9' = do
  let capNum = ord x - ord '0'
  let escapeRegexChar c | c `elem` "^$\\[](){}*+.?" = ['\\',c]
                        | otherwise = [c]
  let escapeRegex = concatMap escapeRegexChar
  replacement <- getCapture capNum
  (escapeRegex replacement ++) <$> subDynamic xs
subDynamic xs = case break (=='%') xs of
                     ([],('%':zs)) -> ('%':) <$> subDynamic zs
                     (ys,(z:zs)) -> (ys ++) <$> subDynamic (z:zs)
                     (ys,[]) -> return ys

getCapture :: Int -> TokenizerM String
getCapture capnum = do
  capts <- gets captures
  if length capts < capnum
     then mzero
     else return $ capts !! (capnum - 1)

-- TODO eventually the keywords need to be a set
-- though this complicates code generation
keyword :: KeywordAttr -> WordSet -> TokenizerM String
keyword kwattr kws = do
  prev <- gets prevChar
  guard $ prev `Set.member` (keywordDelims kwattr)
  inp <- gets input
  let (w,_) = break (`Set.member` (keywordDelims kwattr)) inp
  guard $ not (null w)
  case kws of
       CaseSensitiveWords ws | w `Set.member` ws -> takeChars w
       CaseInsensitiveWords ws | mk w `Set.member` ws -> takeChars w
       _ -> mzero

normalizeHighlighting :: [Token] -> [Token]
normalizeHighlighting [] = []
normalizeHighlighting ((_,""):xs) = normalizeHighlighting xs
normalizeHighlighting ((t,x):xs) =
  (t, concat (x : map snd matches)) : normalizeHighlighting rest
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

