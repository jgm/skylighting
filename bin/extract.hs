{-# LANGUAGE Arrows #-}

import Skylighting.Parser (parseSyntaxDefinition, missingIncludes)
import Text.Show.Pretty (ppShow)
import Skylighting.Types
import System.Exit
import System.Environment (getArgs)
import System.Directory
import Data.List (isInfixOf, intersperse)
import Data.Either (partitionEithers)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  createDirectoryIfMissing True "src/Skylighting/Syntax"
  files <- getArgs
  (errs, syntaxes) <- partitionEithers <$> mapM parseSyntaxDefinition files
  mapM_ (hPutStrLn stderr) errs
  mapM_ writeModuleFor syntaxes

  case missingIncludes syntaxes of
       [] -> return ()
       ns -> do
         mapM_ (\(syn,dep) -> hPutStrLn stderr
             ("Missing syntax definition: " ++ syn ++ " requires " ++
               dep ++ " through IncludeRules.")) ns
         hPutStrLn stderr "Fatal error."
         exitWith (ExitFailure 1)

  putStrLn "Backing up skylighting.cabal to skylighting.cabal.orig"
  copyFile "skylighting.cabal" "skylighting.cabal.orig"

  putStrLn "Updating module list in skylighting.cabal"
  cabalLines <- lines <$> readFile "skylighting.cabal.orig"
  let (top, rest) = break ("other-modules:" `isInfixOf`) cabalLines
  let (_, bottom) = span ("Skylighting.Syntax." `isInfixOf`) (drop 1 rest)
  let modulenames = map (\s -> "Skylighting.Syntax." ++ sShortname s) syntaxes
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
     , "syntaxMap :: SyntaxMap"
     , "syntaxMap = Map.fromList ["
     ] ++
     (intersperse "  , "
       ["  (" ++ show (sName s) ++ ", "
              ++ "Skylighting.Syntax." ++ sShortname s ++ ".syntax)"
                  | s <- syntaxes ]) ++
     ["  ]"]

writeModuleFor :: Syntax -> IO ()
writeModuleFor syn = do
  let fp = toPathName syn
  putStrLn $ "Writing " ++ fp
  let isregex (RegExpr{}) = True
      isregex _ = False
  let iskeyword (Keyword{}) = True
      iskeyword _ = False
  let matchers = map rMatcher $ concatMap cRules $ sContexts syn
  let usesRegex = any isregex matchers
  let usesSet = any iskeyword matchers
  writeFile fp $ unlines $
    [ "module Skylighting.Syntax." ++ sShortname syn ++ " (syntax) where"
    , ""
    , "import Skylighting.Types"
    , "import Data.Map" ] ++
    [ "import Skylighting.Regex" | usesRegex ] ++
    [ "import qualified Data.Set" | usesSet ] ++
    [ ""
    , "syntax :: Syntax"
    , "syntax = " ++ ppShow syn ]

toPathName :: Syntax -> String
toPathName s =
  "src/Skylighting/Syntax/" ++
  map (\c -> if c == '.' then '/' else c)
      (sShortname s) ++ ".hs"
