{-# LANGUAGE Arrows #-}

import Skylighting.Parser (parseSyntaxDefinition)
import Text.Show.Pretty (ppShow)
import System.FilePath
import Data.Char (isAlphaNum)
import Skylighting.Types
import System.Environment (getArgs)
import qualified Data.Map as Map
import System.Directory
import Data.List (isInfixOf, intersperse)

main :: IO ()
main = do
  createDirectoryIfMissing True "Skylighting/Syntax"
  files <- getArgs
  syntaxes <- mapM parseSyntaxDefinition files
  let pairs = zip files syntaxes
  mapM_ writeModuleFor pairs

  putStrLn "Backing up skylighting.cabal to skylighting.cabal.orig"
  copyFile "skylighting.cabal" "skylighting.cabal.orig"

  putStrLn "Updating module list in skylighting.cabal"
  cabalLines <- lines <$> readFile "skylighting.cabal.orig"
  let (top, rest) = break ("other-modules:" `isInfixOf`) cabalLines
  let (_, bottom) = span ("Syntax.Syntax_" `isInfixOf`) (drop 1 rest)
  let modulenames = map toModuleName files
  let autogens = map ((replicate 23 ' ') ++) modulenames
  let newcabal = unlines $ top ++ ("  other-modules:" : autogens) ++ bottom
  writeFile "skylighting.cabal" newcabal

  putStrLn "Writing Skylighting/Syntax.hs"
  writeFile "Skylighting/Syntax.hs" $ unlines $
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
              ++ toModuleName f ++ ".syntax)" | (f,s) <- pairs ]) ++
     ["  ]"]

writeModuleFor :: (String, Syntax) -> IO ()
writeModuleFor (s, syn) = do
  let fp = toPathName s
  putStrLn $ "Writing " ++ fp
  writeFile fp $
      "module " ++ toModuleName s ++ " (syntax) where\n\nimport Skylighting.Types\nimport Skylighting.Regex\nimport Data.Map\nimport qualified Data.Set\n\nsyntax :: Syntax\nsyntax = " ++ ppShow syn

toModuleName :: String -> String
toModuleName s = "Skylighting.Syntax.Syntax_" ++
  map (\c -> if isAlphaNum c then c else '_') (takeBaseName s)

toPathName :: String -> String
toPathName s = map (\c -> if c == '.' then '/' else c)
                      (toModuleName (dropExtension s)) ++ ".hs"

