{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where
import Skylighting
import Text.Show.Pretty
import Data.Char (toLower)
import Control.Monad
import System.Exit
import System.Directory
import System.FilePath
import Data.Maybe (fromMaybe)
import Text.Printf
import System.IO
import Data.Monoid (mempty)
import Text.Printf
import Data.Algorithm.Diff
import Control.Applicative
import System.Environment (getArgs)
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)

main :: IO ()
main = do
  inputs <- filter (\fp -> take 1 fp /= ".")
         <$> getDirectoryContents ("test" </> "cases")
  args <- getArgs
  let regen = "--regenerate" `elem` args
  defaultMain $ testGroup "Golden tests" $ map (mkTest regen) inputs

mkTest :: Bool -> FilePath -> TestTree
mkTest regen inpFile = localOption (mkTimeout 2000000) $
  goldenTest testname getExpected getActual compareValues updateGolden
  where testname = lang ++ " highlighting of " ++ inpFile
        getExpected = readFile (expecteddir </> inpFile <.> "native")
        getActual = do
          code <- readFile (casesdir </> inpFile)
          syntax <- case lookupSyntax lang defaultSyntaxMap of
                         Just s  -> return s
                         Nothing -> fail $
                            "Could not find syntax definition for " ++ lang
          case tokenize TokenizerConfig{
                             traceOutput = False
                           , syntaxMap = defaultSyntaxMap } syntax $! code of
                 Left e -> fail e
                 Right ls -> return $ ppShow ls ++ "\n"
        opts = defaultFormatOpts{ titleAttributes = False }
        updateGolden = if regen
                          then writeFile (expecteddir </> inpFile <.> "native")
                          else \_ -> return ()
        expecteddir = "test" </> "expected"
        casesdir = "test" </> "cases"
        lang = drop 1 $ takeExtension inpFile
        compareValues expected actual = do
           if expected == actual
              then return Nothing
              else return $ Just $ unlines $
                   [ "--- " ++ (expecteddir </> inpFile <.> "native")
                   , "+++ actual" ] ++
                   map vividize (getDiff (lines expected) (lines actual))

vividize :: Diff String -> String
vividize (Both s _) = "  " ++ s
vividize (First s)  = "- " ++ s
vividize (Second s) = "+ " ++ s
