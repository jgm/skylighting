module Skylighting.Tokenizer (
  tokenize
  ) where

import Skylighting.Types
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

data TokenizerState = TokenizerState{
    input        :: String
  , contextStack :: [Context]
  , captures     :: [String]
  , column       :: Int
} deriving (Show)

type TokenizerM = ExceptT String (State TokenizerState)

tokenize :: Syntax -> String -> Either String [SourceLine]
tokenize syntax inp = evalState (runExceptT $ mapM tokenizeLine $ lines inp)
  TokenizerState{ input = inp
                , contextStack = [sStartingContext syntax]
                , captures = []
                , column = 0 }

tokenizeLine :: String -> TokenizerM SourceLine
tokenizeLine ln = do
  modify $ \st -> st{ input = ln }
  many getToken

getToken :: TokenizerM Token
getToken = do
  inp <- gets input
  case inp of
     []     -> mzero
     (c:cs) -> do
       modify $ \st -> st{ input = cs }
       return (ErrorTok, [c])

