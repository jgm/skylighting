{-# LANGUAGE CPP #-}
module Skylighting.Tokenizer (
  tokenize
  ) where

import qualified Data.Set as Set
import Skylighting.Regex
import Skylighting.Types
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Debug.Trace

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

tokenize :: Syntax -> String -> Either String [SourceLine]
tokenize syntax inp = evalState (runExceptT $ mapM tokenizeLine $ lines inp)
  TokenizerState{ input = inp
                , contextStack = ContextStack [sStartingContext syntax]
                , captures = []
                , column = 0 }

tokenizeLine :: String -> TokenizerM [Token]
tokenizeLine ln = do
  modify $ \st -> st{ input = ln }
  ts <- many getToken
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
  msum (map tryRule (cRules context))

takeChars :: String -> TokenizerM String
takeChars [] = mzero
takeChars xs = do
  modify $ \st -> st{ input = drop (length xs) (input st) }
  return xs

tryRule :: Rule -> TokenizerM Token
tryRule rule = do
  info $ "Trying " ++ take 12 (show (rMatcher rule))
  xs <- case rMatcher rule of
             DetectChar c -> detectChar c
             Detect2Chars c d -> detect2Chars c d
             AnyChar cs -> anyChar cs
             RegExpr re -> regExpr re
             Keyword kwattr kws -> keyword kwattr kws
             _ -> do
               mzero
  let attr = rAttribute rule
  return (attr, xs)

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

