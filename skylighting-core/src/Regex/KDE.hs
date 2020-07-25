{-# LANGUAGE OverloadedStrings #-}
module Regex.KDE
 (Regex(..), compileRegex, matchRegex, testRegex)
  where

import Regex.KDE.Regex
import Regex.KDE.Compile
import Regex.KDE.Match
import qualified Data.ByteString.UTF8 as U
import qualified Data.IntMap.Strict as M
import qualified Data.ByteString as B
import Data.List (sortOn)

testRegex :: Bool -> String -> String -> Maybe (String, [(Int, String)])
testRegex caseSensitive re s =
  let bs = U.fromString s
      toSlice (off,len) = U.toString $ B.take len $ B.drop off bs
   in case compileRegex caseSensitive (U.fromString re) of
        Right r ->
          case matchRegex r bs of
            Nothing -> Nothing
            Just (m,cs) -> Just (U.toString m, sortOn fst
                                  (M.toList (M.map toSlice cs)))
        Left e  -> error e
