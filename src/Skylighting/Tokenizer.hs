{-# LANGUAGE TupleSections #-}
module Skylighting.Tokenizer (
    tokenize
  , tokenizeWithTrace
  ) where

import qualified Data.Set as Set
import Skylighting.Regex
import Skylighting.Types
import Skylighting.Syntax (syntaxMap)
import Data.Maybe
import Data.List (isPrefixOf, findIndex)
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Data.Char (isSpace, isLetter, isAlphaNum)
import qualified Data.Map as Map
import Debug.Trace

info :: String -> TokenizerM ()
info s = do
  tr <- gets traceOutput
  when tr $ trace s (return ())

infoContextStack :: TokenizerM ()
infoContextStack = do
  tr <- gets traceOutput
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
  , traceOutput  :: Bool
  , lookaheadRule :: Bool
} deriving (Show)

type TokenizerM = ExceptT String (State TokenizerState)

popContextStack :: TokenizerM ()
popContextStack = do
  ContextStack cs <- gets contextStack
  case cs of
       []     -> error "Empty context stack" -- programming error
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
       []    -> error "Empty context stack" -- programming error
       (c:_) -> return c

doContextSwitch :: [ContextSwitch] -> TokenizerM ()
doContextSwitch [] = return ()
doContextSwitch (Pop : xs) = popContextStack >> doContextSwitch xs
doContextSwitch (Push (syn,c) : xs) = do
  case Map.lookup syn syntaxMap >>= lookupContext c of
       Just con -> pushContextStack con >> doContextSwitch xs
       Nothing  -> error $"Unknown syntax or context: " ++ show (syn, c) -- TODO handle better

lookupContext :: String -> Syntax -> Maybe Context
lookupContext name syntax =
  if null name
     then Just $ sStartingContext syntax
     else Map.lookup name $ sContexts syntax


tokenize :: Syntax -> String -> Either String [SourceLine]
tokenize syntax inp = evalState (runExceptT $ mapM tokenizeLine $ lines inp)
  startingState{ input = inp, contextStack = ContextStack [sStartingContext syntax] }

tokenizeWithTrace :: Syntax -> String -> Either String [SourceLine]
tokenizeWithTrace syntax inp = evalState (runExceptT $ mapM tokenizeLine $ lines inp)
  startingState{ input = inp, contextStack = ContextStack [sStartingContext syntax],
                 traceOutput = True }

startingState :: TokenizerState
startingState =
  TokenizerState{ input = ""
                , prevChar = '\n'
                , contextStack = ContextStack []
                , captures = []
                , column = 0
                , lineContinuation = False
                , firstNonspaceColumn = Nothing
                , traceOutput = False
                , lookaheadRule = False }

tokenizeLine :: String -> TokenizerM [Token]
tokenizeLine ln = do
  cur <- currentContext
  lineCont <- gets lineContinuation
  if lineCont
     then modify $ \st -> st{ lineContinuation = False }
     else do
       modify $ \st -> st{ column = 0
                         , firstNonspaceColumn =
                              findIndex (not . isSpace) ln }
       doContextSwitch (cLineBeginContext cur)
  doContextSwitch (cLineBeginContext cur)
  modify $ \st -> st{ input = ln, prevChar = '\n' }
  ts <- normalizeHighlighting <$> many getToken
  cur' <- currentContext
  lineCont' <- gets lineContinuation
  unless lineCont' $ doContextSwitch (cLineEndContext cur')
  inp <- gets input
  return $ ts ++ [(ErrorTok, inp) | not (null inp)]

getToken :: TokenizerM Token
getToken = do
  inp <- gets input
  guard $ not (null inp)
  context <- currentContext
  msum (map tryRule (cRules context)) <|>
    if cFallthrough context
       then doContextSwitch (cFallthroughContext context) >> getToken
       else (cAttribute context, ) <$> normalChunk

takeChars :: String -> TokenizerM String
takeChars [] = mzero
takeChars xs = do
  let numchars = length xs
  lookahead <- gets lookaheadRule
  if lookahead
     then return ""
     else do
       modify $ \st -> st{ input = drop numchars (input st),
                           prevChar = last xs,
                           column = column st + numchars }
       return xs

tryRule :: Rule -> TokenizerM Token
tryRule rule = do
  when (rLookahead rule) $
    modify (\st -> st{ lookaheadRule = True })
  let attr = rAttribute rule
  (tt, s) <- case rMatcher rule of
                DetectChar c -> withAttr attr $ detectChar c
                Detect2Chars c d -> withAttr attr $ detect2Chars c d
                AnyChar cs -> withAttr attr $ anyChar cs
                RangeDetect c d -> withAttr attr $ rangeDetect c d
                RegExpr re -> withAttr attr $ regExpr re
                Int -> withAttr attr $ regExpr integerRegex
                HlCOct -> withAttr attr $ regExpr octRegex
                HlCHex -> withAttr attr $ regExpr hexRegex
                HlCStringChar -> withAttr attr $ regExpr hlCStringCharRegex
                HlCChar -> withAttr attr $ regExpr hlCCharRegex
                Float -> withAttr attr $ regExpr floatRegex
                Keyword kwattr kws ->
                  withAttr attr $ keyword kwattr kws
                StringDetect s -> withAttr attr $ stringDetect s
                WordDetect s -> withAttr attr $ wordDetect s
                LineContinue -> withAttr attr $ lineContinue
                DetectSpaces -> withAttr attr $ detectSpaces
                DetectIdentifier -> withAttr attr $ detectIdentifier
                IfFirstNonspace r -> ifFirstNonspace r
                IfColumn n r -> ifColumn n r
                IncludeRules cname -> includeRules
                   (if rIncludeAttribute rule then Just attr else Nothing)
                   cname
  (_, cresult) <- msum (map tryRule (rChildren rule))
              <|> return (NormalTok, "")
  modify (\st -> st{ lookaheadRule = False })
  let tok = (tt, s ++ cresult)
  info $ takeWhile (/=' ') (show (rMatcher rule)) ++ " MATCHED " ++ show tok
  doContextSwitch (rContextSwitch rule)
  return tok

withAttr :: TokenType -> TokenizerM String -> TokenizerM Token
withAttr tt p = (tt,) <$> p

hlCStringCharRegex :: RE
hlCStringCharRegex = RE{
    reString = reStr
  , reCompiled = Just $ compileRegex False reStr
  , reDynamic  = False
  , reCaseSensitive = False
  }
  where reStr = "\\\\(?[abefnrtv\"'?\\\\]|[xX][a-fA-F0-9]+|0[0-7]+)"

hlCCharRegex :: RE
hlCCharRegex = RE{
    reString = reStr
  , reCompiled = Just $ compileRegex False reStr
  , reDynamic  = False
  , reCaseSensitive = False
  }
  where reStr = "'\\\\(?[abefnrtv\"'?\\\\]|[xX][a-fA-F0-9]+|0[0-7]+)'"

wordDetect :: String -> TokenizerM String
wordDetect s = do
  res <- stringDetect s
  -- now check for word boundary:  (TODO: check to make sure this is correct)
  inp <- gets input
  case inp of
       (c:_) | not (isAlphaNum c) -> return res
       _ -> mzero

ifColumn :: Int -> Rule -> TokenizerM Token
ifColumn n rule = do
  col <- gets column
  guard $ col == n
  tryRule rule

ifFirstNonspace :: Rule -> TokenizerM Token
ifFirstNonspace rule = do
  firstNonspace <- gets firstNonspaceColumn
  col <- gets column
  guard $ firstNonspace == Just col
  tryRule rule

stringDetect :: String -> TokenizerM String
stringDetect s = do
  inp <- gets input
  if s `isPrefixOf` inp
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

includeRules :: Maybe TokenType -> ContextName -> TokenizerM Token
includeRules mbattr (syn, con) = do
  (t,xs) <- case Map.lookup syn syntaxMap >>= lookupContext con of
                 Nothing  -> error $ "Context lookup failed " ++ show (syn, con)
                 Just c   -> msum (map tryRule (cRules c))
  return (fromMaybe t mbattr, xs)

detectChar :: Char -> TokenizerM String
detectChar c = do
  inp <- gets input
  case inp of
    (x:_) | x == c -> takeChars [x]
    _ -> mzero

detect2Chars :: Char -> Char -> TokenizerM String
detect2Chars c d = do
  inp <- gets input
  case inp of
    (x:y:_) | x == c && y == d -> takeChars [x,y]
    _ -> mzero

rangeDetect :: Char -> Char -> TokenizerM String
rangeDetect c d = do
  inp <- gets input
  case inp of
    (x:rest) | x == c -> takeChars (takeWhile (/= d) rest ++ [d])
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
      takeChars $ [c] ++ takeWhile (\d -> isAlphaNum d || d == '_') cs
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

regExpr :: RE -> TokenizerM String
regExpr re = do -- TODO dynamic, case sensitive
  -- TODO if dynamic, modify reString here?
  regex <- maybe (return $ compileRegex (reCaseSensitive re) (reString re))
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

-- TODO eventually the keywords need to be a set
-- though this complicates code generation
keyword :: KeywordAttr -> WordSet -> TokenizerM String
keyword kwattr (WordSet kws) = do
  inp <- gets input
  let (w,_) = break (`Set.member` (keywordDelims kwattr)) inp
  guard $ not (null w)
  -- TODO handle keywordCaseInsensitive
  if w `Set.member` kws
     then takeChars w
     else mzero

-- TODO better as a monad instance for token
-- perhaps use Seq?
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
  , reDynamic  = False
  , reCaseSensitive = False
  }
  where intReStr = "\\b[-+]?(0[Xx][0-9A-Fa-f]+|0[Oo][0-7]+|[0-9]+)\\b"

floatRegex :: RE
floatRegex = RE{
    reString = floatReStr
  , reCompiled = Just $ compileRegex False floatReStr
  , reDynamic = False
  , reCaseSensitive = False
  }
  where floatReStr = "\\b[-+]?(([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+)\\b"

octRegex :: RE
octRegex = RE{
    reString = octRegexStr
  , reCompiled = Just $ compileRegex False octRegexStr
  , reDynamic = False
  , reCaseSensitive = False
  }
  where octRegexStr = "\\b[-+]?0[Oo][0-7]+\\b"

hexRegex :: RE
hexRegex = RE{
    reString = hexRegexStr
  , reCompiled = Just $ compileRegex False hexRegexStr
  , reDynamic = False
  , reCaseSensitive = False
  }
  where hexRegexStr = "\\b[-+]?0[Xx][0-9A-Fa-f]+\\b"


