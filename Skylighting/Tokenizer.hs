{-# LANGUAGE CPP, TupleSections #-}
module Skylighting.Tokenizer (
  tokenize
  ) where

import qualified Data.Set as Set
import Skylighting.Regex
import Skylighting.Types
import Skylighting.Syntax (syntaxMap)
import Data.Maybe
import Data.List (isPrefixOf)
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Debug.Trace
import Data.Char (isSpace, isLetter, isAlphaNum)
import qualified Data.Map as Map

info :: String -> TokenizerM ()
#ifdef TRACE
info s = trace s (return ())
#else
info _ = return ()
#endif

infoContextStack :: TokenizerM ()
infoContextStack = do
  ContextStack stack <- gets contextStack
  info $ show $ map cName stack

newtype ContextStack = ContextStack{ unContextStack :: [Context] }
  deriving (Show)

data TokenizerState = TokenizerState{
    input        :: String
  , prevChar     :: Char
  , contextStack :: ContextStack
  , captures     :: [String]
  , column       :: Int
  , lineContinuation :: Bool
} deriving (Show)

type TokenizerM = ExceptT String (State TokenizerState)

popContextStack :: TokenizerM ()
popContextStack = do
  ContextStack cs <- gets contextStack
  case cs of
       []     -> error "Empty context stack" -- programming error
       [_]    -> return ()  -- don't pop last context
       (_:rest) -> modify (\st -> st{ contextStack = ContextStack rest })

pushContextStack :: Context -> TokenizerM ()
pushContextStack cont =
  modify (\st -> st{ contextStack =
                      ContextStack (cont : unContextStack (contextStack st)) } )

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
  case Map.lookup syn syntaxMap >>= Map.lookup c . sContexts of
       Just con -> pushContextStack con >> doContextSwitch xs
       Nothing  -> error $"Unknown syntax or context: " ++ show (syn, c) -- TODO handle better

tokenize :: Syntax -> String -> Either String [SourceLine]
tokenize syntax inp = evalState (runExceptT $ mapM tokenizeLine $ lines inp)
  TokenizerState{ input = inp
                , prevChar = '\n'
                , contextStack = ContextStack [sStartingContext syntax]
                , captures = []
                , column = 0
                , lineContinuation = False }

tokenizeLine :: String -> TokenizerM [Token]
tokenizeLine ln = do
  cur <- currentContext
  lineCont <- gets lineContinuation
  if lineCont
     then modify $ \st -> st{ lineContinuation = False }
     else doContextSwitch (cLineEndContext cur)
  doContextSwitch (cLineBeginContext cur)
  modify $ \st -> st{ input = ln, prevChar = '\n' }
  ts <- normalizeHighlighting <$> many getToken
  if lineCont
     then return ()
     else doContextSwitch (cLineEndContext cur)
  inp <- gets input
  return $ ts ++ [(ErrorTok, inp) | not (null inp)]

getToken :: TokenizerM Token
getToken = do
  inp <- gets input
  guard $ not (null inp)
  context <- currentContext
  infoContextStack
  info $ "At :" ++ take 5 inp
  msum (map tryRule (cRules context)) <|> -- TODO check for fallthrough
    if cFallthrough context
       then doContextSwitch (cFallthroughContext context) >> getToken
       else (cAttribute context, ) <$> nextChar

takeChars :: String -> TokenizerM String
takeChars [] = mzero
takeChars xs = do
  modify $ \st -> st{ input = drop (length xs) (input st),
                      prevChar = last xs }
  return xs

tryRule :: Rule -> TokenizerM Token
tryRule rule = do
  -- info $ "Trying " ++ take 12 (show (rMatcher rule))
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
                HlCStringChar -> mzero -- TODO
                HlCChar -> mzero -- TODO
                Float -> withAttr attr $ regExpr floatRegex
                Keyword kwattr kws ->
                  withAttr attr $ keyword kwattr kws
                StringDetect s -> withAttr attr $ stringDetect s
                LineContinue -> withAttr attr $ lineContinue
                DetectSpaces -> withAttr attr $ detectSpaces
                DetectIdentifier -> withAttr attr $ detectIdentifier
                IfFirstNonspace r -> mzero -- TODO
                IfColumn n r -> mzero -- TODO
                IncludeRules cname -> includeRules
                   (if rIncludeAttribute rule then Just attr else Nothing)
                   cname
                Unimplemented str -> do
                  info $ "Unimplemented matcher: " ++ show str
                  mzero
  (_, cresult) <- msum (map tryRule (rChildren rule))
              <|> return (NormalTok, "")
  doContextSwitch (rContextSwitch rule)
  return (tt, s ++ cresult)

withAttr :: TokenType -> TokenizerM String -> TokenizerM Token
withAttr tt p = (tt,) <$> p

stringDetect :: String -> TokenizerM String
stringDetect s = do
  inp <- gets input
  if s `isPrefixOf` inp
     then takeChars s
     else mzero

nextChar :: TokenizerM String
nextChar = do
  inp <- gets input
  case inp of
    (x:_) -> takeChars [x]
    _ -> mzero

includeRules :: Maybe TokenType -> ContextName -> TokenizerM Token
includeRules mbattr (syn, con) = do
  (t,xs) <- case Map.lookup syn syntaxMap >>= Map.lookup con . sContexts of
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
normalizeHighlighting ((a,x):(b,y):xs)
  | a == b = normalizeHighlighting ((a, x++y):xs)
normalizeHighlighting (x:xs) = x : normalizeHighlighting xs

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


