{-# LANGUAGE Arrows #-}

import Data.Either (partitionEithers)
import Data.List (intercalate, isInfixOf)
import qualified Data.Text as Text
import Skylighting.Parser (missingIncludes, parseSyntaxDefinition)
import Skylighting.Types
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)
import Data.Binary (encode)

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
             ("Missing syntax definition: " ++ Text.unpack syn ++ " requires " ++
               Text.unpack dep ++ " through IncludeRules.")) ns
         hPutStrLn stderr "Fatal error."
         exitWith (ExitFailure 1)

  putStrLn "Backing up skylighting.cabal to skylighting.cabal.orig"
  copyFile "skylighting.cabal" "skylighting.cabal.orig"

  putStrLn "Updating module list in skylighting.cabal"
  cabalLines <- lines <$> readFile "skylighting.cabal.orig"
  let (top, rest) = break ("other-modules:" `isInfixOf`) cabalLines
  let (_, bottom) = span ("Skylighting.Syntax." `isInfixOf`) (drop 1 rest)
  let modulenames = map (\s -> "Skylighting.Syntax." ++
                          Text.unpack (sShortname s)) syntaxes
  let autogens = map ((replicate 23 ' ') ++) modulenames
  let newcabal = unlines $ top ++ ("  other-modules:" : autogens) ++ bottom
  writeFile "skylighting.cabal" newcabal

  putStrLn "Writing src/Skylighting/Syntax.hs"
  writeFile "src/Skylighting/Syntax.hs" $ unlines (
     [ "{-# LANGUAGE OverloadedStrings #-}"
     , "module Skylighting.Syntax (defaultSyntaxMap) where"
     , "import qualified Data.Map as Map"
     , "import Skylighting.Types" ] ++
     [ "import qualified " ++ m | m <- modulenames ]
     ++
     [ ""
     , "defaultSyntaxMap :: SyntaxMap"
     , "defaultSyntaxMap = Map.fromList ["
     ]) ++ "   " ++
     (intercalate "\n  ,"
       ["  (" ++ show (Text.unpack $ sName s) ++ ", "
              ++ "Skylighting.Syntax." ++ Text.unpack (sShortname s) ++ ".syntax)"
                  | s <- syntaxes ]) ++ " ]"

writeModuleFor :: Syntax -> IO ()
writeModuleFor syn = do
  let fp = toPathName syn
  putStrLn $ "Writing " ++ fp
  writeFile fp $ unlines $
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "module Skylighting.Syntax." ++ Text.unpack (sShortname syn) ++
        " (syntax) where"
    , ""
    , "import Skylighting.Types"
    , "import Data.Binary"
    , ""
    , "syntax :: Syntax"
    , "syntax = decode " ++ show (encode syn) ]

-- NOTE:  we include string representation of the Syntax,
-- which we then 'decode', rather than the code for the Syntax,
-- because ghc doesn't deal well with large data structure
-- literals.  For background see jgm/skylighting#7 and
--  http://stackoverflow.com/questions/16348340/compiling-very-large-constants-with-ghc

toPathName :: Syntax -> String
toPathName s =
  "src/Skylighting/Syntax/" ++
  map (\c -> if c == '.' then '/' else c)
      (Text.unpack (sShortname s)) ++ ".hs"
