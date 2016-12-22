{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
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
import Test.Tasty.HUnit
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  inputs <- filter (\fp -> take 1 fp /= ".")
         <$> getDirectoryContents ("test" </> "cases")
  args <- getArgs
  let regen = "--regenerate" `elem` args
  defaultTheme <- BL.readFile ("test" </> "default.theme")
  defaultMain $ testGroup "skylighting tests" $
    [ testGroup "tokenizer tests" $
        map (tokenizerTest regen) inputs
    , testGroup "FromJSON instance tests"
       [ testCase "decode simple color" $
            Just (RGB 0x15 0xff 0xa0) @=? decode "\"#15ffa0\""
       , testCase "decode TokenStyle" $
            Just (TokenStyle{tokenColor = Just (RGB 0x1f 0x1c 0x1b),
                             tokenBackground = Nothing,
                             tokenBold = True,
                             tokenItalic = False,
                             tokenUnderline = False }) @=?
            decode "{ \"text-color\": \"#1f1c1b\", \"bold\": true }"
       , testCase "decode KDE theme to Style" $
            Just kate @=? decode defaultTheme
       ]
    ]

compareValues :: FilePath -> String -> String -> IO (Maybe String)
compareValues referenceFile expected actual =
   if expected == actual
      then return $ Nothing
      else return $ Just $ unlines $
           [ "--- " ++ referenceFile
           , "+++ actual" ] ++
           map vividize (getDiff (lines expected) (lines actual))

tokenizerTest :: Bool -> FilePath -> TestTree
tokenizerTest regen inpFile = localOption (mkTimeout 2000000) $
  goldenTest testname getExpected getActual
      (compareValues referenceFile) updateGolden
  where testname = lang ++ " tokenizing of " ++ inpFile
        getExpected = readFile referenceFile
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
                          then writeFile referenceFile
                          else \_ -> return ()
        expecteddir = "test" </> "expected"
        casesdir = "test" </> "cases"
        referenceFile = expecteddir </> inpFile <.> "native"
        lang = drop 1 $ takeExtension inpFile

vividize :: Diff String -> String
vividize (Both s _) = "  " ++ s
vividize (First s)  = "- " ++ s
vividize (Second s) = "+ " ++ s
