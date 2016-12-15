module Skylighting.Tokenizer (

  ) where

import Skylighting.Types

data TokenizerState = TokenizerState{
    input        :: String
  , contextStack :: (SyntaxName, ContextName)
  , captures     :: [String]
  , column       :: Int
} deriving (Show)


