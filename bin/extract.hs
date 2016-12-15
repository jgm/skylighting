{-# LANGUAGE Arrows #-}

import Skylighting.Parser (parseSyntaxDefinition)
import Text.Show.Pretty (ppShow)
import Skylighting.Types
import System.Environment (getArgs)
import qualified Data.Map as Map

main :: IO ()
main = do
  syntaxes <- getArgs >>= mapM parseSyntaxDefinition
  let syntaxMap = Map.fromList [(sName s, s) | s <- syntaxes]
  putStrLn $
      "module Skylighting.Syntax (syntaxMap) where\nimport Skylighting.Types\nimport Skylighting.Regex\nimport Data.Map\nimport qualified Data.Set\n\nsyntaxMap :: Data.Map.Map String Syntax\nsyntaxMap = " ++ ppShow syntaxMap

