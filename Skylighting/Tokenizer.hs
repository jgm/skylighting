{-# LANGUAGE CPP #-}
module Skylighting.Tokenizer (
  tokenize
  ) where

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

tokenizeLine :: String -> TokenizerM SourceLine
tokenizeLine ln = do
  modify $ \st -> st{ input = ln }
  many getToken

getToken :: TokenizerM Token
getToken = do
  inp <- gets input
  context <- currentContext
  msum $ map tryRule (cRules context)
  {-
  go (cRules context)
    where go (r:rs) = (do
            t <- tryRule r
            return t) <|> go rs
          go [] = mzero
  -}

tryRule :: Rule -> TokenizerM Token
tryRule rule = do
  info $ "trying " ++ show rule
  inp <- gets input
  case inp of
     []     -> mzero
     (c:cs) -> do
       modify $ \st -> st{ input = cs }
       return (ErrorTok, [c])
