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
  let isregex (RegExpr{}) = True
      isregex _ = False
  let iskeyword (Keyword{}) = True
      iskeyword _ = False
  let matchers = map rMatcher $ concatMap cRules $ sContexts syn
  let usesRegex = any isregex matchers
  let usesSet = any iskeyword matchers
  writeFile fp $ unlines $
    [ "module Skylighting.Syntax." ++ sName syn ++ " (syntax) where"
    , ""
    , "import Skylighting.Types"
    , "import Data.Map" ] ++
    [ "import Skylighting.Regex" | usesRegex ] ++
    [ "import qualified Data.Set" | usesSet ] ++
    [ ""
    , "syntax :: Syntax"
    , "syntax = " ++ ppShow syn ]

toPathName :: String -> String
toPathName s = "src/Skylighting/Syntax/" ++ map (\c -> if c == '.' then '/' else c) s ++ ".hs"

