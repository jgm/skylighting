{-# LANGUAGE Arrows #-}

import Skylighting.Parser (parseSyntaxDefinition)
import Text.Show.Pretty (ppShow)
import Skylighting.Types
import System.Environment (getArgs)
import System.Directory
import Data.List (isInfixOf, intersperse)

main :: IO ()
main = do
  createDirectoryIfMissing True "Skylighting/Syntax"
  files <- getArgs
  syntaxes <- mapM parseSyntaxDefinition files
  mapM_ writeModuleFor syntaxes

  putStrLn "Backing up skylighting.cabal to skylighting.cabal.orig"
  copyFile "skylighting.cabal" "skylighting.cabal.orig"

  putStrLn "Updating module list in skylighting.cabal"
  cabalLines <- lines <$> readFile "skylighting.cabal.orig"
  let (top, rest) = break ("other-modules:" `isInfixOf`) cabalLines
  let (_, bottom) = span ("Skylighting.Syntax." `isInfixOf`) (drop 1 rest)
  let modulenames = map (\s -> "Skylighting.Syntax." ++ sName s) syntaxes
  let autogens = map ((replicate 23 ' ') ++) modulenames
  let newcabal = unlines $ top ++ ("  other-modules:" : autogens) ++ bottom
  writeFile "skylighting.cabal" newcabal

  putStrLn "Writing src/Skylighting/Syntax.hs"
  writeFile "src/Skylighting/Syntax.hs" $ unlines $
     [ "module Skylighting.Syntax (syntaxMap) where"
     , "import qualified Data.Map as Map"
     , "import Skylighting.Types" ] ++
     [ "import qualified " ++ m | m <- modulenames ]
     ++
     [ ""
     , "syntaxMap :: Map.Map String Syntax"
     , "syntaxMap = Map.fromList ["
     ] ++
     (intersperse "  , "
       ["  (" ++ show (sName s) ++ ", "
              ++ "Skylighting.Syntax." ++ sName s
              ++ ".syntax)" | s <- syntaxes ]) ++
     ["  ]"]

writeModuleFor :: Syntax -> IO ()
writeModuleFor syn = do
  let fp = toPathName $ sName syn
  putStrLn $ "Writing " ++ fp
  writeFile fp $
      "module Skylighting.Syntax." ++ sName syn ++ " (syntax) where\n\nimport Skylighting.Types\nimport Skylighting.Regex\nimport Data.Map\nimport qualified Data.Set\n\nsyntax :: Syntax\nsyntax = " ++ ppShow syn

toPathName :: String -> String
toPathName s = "src/Skylighting/Syntax/" ++ map (\c -> if c == '.' then '/' else c) s ++ ".hs"

