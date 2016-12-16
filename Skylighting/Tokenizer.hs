{-# LANGUAGE CPP #-}
module Skylighting.Tokenizer (
  tokenize
  ) where

import qualified Data.Set as Set
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
  many getToken

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
    <|> takeChars ErrorTok inp

takeChars :: TokenType -> String -> TokenizerM Token
takeChars attr xs = do
  inp <- gets input
  modify $ \st -> st{ input = drop (length xs) (input st) }
  return (attr, xs)

tryRule :: Rule -> TokenizerM Token
tryRule rule = do
  -- info $ "trying " ++ show rule
  inp <- gets input
  let attr = rAttribute rule
  case rMatcher rule of
       DetectChar c ->
          case inp of
            (x:_) | x == c ->
              takeChars attr [x]
            _ -> mzero
       Detect2Chars c d ->
          case inp of
            (x1:x2:xs) | x1 == c && x2 == d ->
              takeChars attr [x1,x2]
            _ -> mzero
       AnyChar cs ->
          case inp of
            (x:xs) | x `elem` cs ->
              takeChars attr [x]
            _ -> mzero
       _ -> mzero
