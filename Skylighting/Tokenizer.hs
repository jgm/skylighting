{-# LANGUAGE CPP, TupleSections #-}
module Skylighting.Tokenizer (
  tokenize
  ) where

import qualified Data.Set as Set
import Skylighting.Regex
import Skylighting.Types
import Skylighting.Syntax (syntaxMap)
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Debug.Trace
import qualified Data.Map as Map

info :: String -> TokenizerM ()
#ifdef TRACE
info s = trace s (return ())
#else
info s = return ()
#endif

newtype ContextStack = ContextStack{ unContextStack :: [Context] }
  deriving (Show)

data TokenizerState = TokenizerState{
    input        :: String
  , contextStack :: ContextStack
  , captures     :: [String]
  , column       :: Int
} deriving (Show)

type TokenizerM = ExceptT String (State TokenizerState)

popContextStack :: TokenizerM ()
popContextStack = do
  ContextStack cs <- gets contextStack
  case cs of
       []     -> error "Empty context stack" -- programming error
       [c]    -> return ()  -- don't pop last context
       (c:cs) -> modify (\st -> st{ contextStack = ContextStack cs })

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
doContextSwitch (Push (s,c) : xs) = do
  cur <- currentContext
  let syn = if null s then cSyntax cur else s
  case Map.lookup syn syntaxMap >>= Map.lookup c . sContexts of
       Just con -> pushContextStack con >> doContextSwitch xs
       Nothing  -> error $"Unknown syntax or context: " ++ show (syn, c) -- TODO handle better

tokenize :: Syntax -> String -> Either String [SourceLine]
tokenize syntax inp = evalState (runExceptT $ mapM tokenizeLine $ lines inp)
  TokenizerState{ input = inp
                , contextStack = ContextStack [sStartingContext syntax]
                , captures = []
                , column = 0 }

tokenizeLine :: String -> TokenizerM [Token]
tokenizeLine ln = do
  modify $ \st -> st{ input = ln }
  ts <- normalizeHighlighting <$> many getToken
  inp <- gets input
  return $ ts ++ [(ErrorTok, inp) | not (null inp)]

getToken :: TokenizerM Token
getToken = do
  inp <- gets input
  guard $ not (null inp)
  context <- currentContext
  -- DEBUG
  cstack <- gets contextStack
  info $ "[" ++ unwords (map (show . cName) $ unContextStack cstack) ++  "]"
  --
  msum (map tryRule (cRules context)) <|> -- TODO check for fallthrough
    if cFallthrough context
       then mzero -- TODO
       else (cAttribute context, ) <$> nextChar

takeChars :: String -> TokenizerM String
takeChars [] = mzero
takeChars xs = do
  modify $ \st -> st{ input = drop (length xs) (input st) }
  return xs

tryRule :: Rule -> TokenizerM Token
tryRule rule = do
  info $ "Trying " ++ take 12 (show (rMatcher rule))
  let attr = rAttribute rule
  tok <- case rMatcher rule of
             DetectChar c -> withAttr attr $ detectChar c
             Detect2Chars c d -> withAttr attr $ detect2Chars c d
             AnyChar cs -> withAttr attr $ anyChar cs
             RegExpr re -> withAttr attr $ regExpr re
             Keyword kwattr kws -> withAttr attr $ keyword kwattr kws
             IncludeRules cname -> includeRules
                (if rIncludeAttribute rule then Nothing else Just attr)
                cname
             _ -> mzero
  -- TODO rChildren
  doContextSwitch (rContextSwitch rule)
  return tok

withAttr :: TokenType -> TokenizerM String -> TokenizerM Token
withAttr tt p = (tt,) <$> p

nextChar :: TokenizerM String
nextChar = do
  inp <- gets input
  case inp of
    (x:_) -> takeChars [x]
    _ -> mzero

includeRules :: Maybe TokenType -> ContextName -> TokenizerM Token
includeRules mbattr (syn, con) = do
  syn' <- if null syn
             then cSyntax <$> currentContext
             else return syn
  (t,xs) <- case Map.lookup syn' syntaxMap >>= Map.lookup con . sContexts of
                 Nothing  -> error $ "Context lookup failed " ++ show (syn',con)
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

-- TODO eventually make this a set of Char
anyChar :: [Char] -> TokenizerM String
anyChar cs = do
  inp <- gets input
  case inp of
     (x:xs) | x `elem` cs -> takeChars [x]
     _ -> mzero

regExpr :: RE -> TokenizerM String
regExpr re = mzero -- TODO for now

-- TODO eventually the keywords need to be a set
-- though this complicates code generation
keyword :: KeywordAttr -> [String] -> TokenizerM String
keyword kwattr kws = do
  inp <- gets input
  let (w,_) = break (`Set.member` (keywordDelims kwattr)) inp
  guard $ not (null w)
  -- TODO handle keywordCaseInsensitive
  if w `elem` kws
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

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace '\r' = True
isSpace _    = False
