{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where
import Skylighting
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
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data TestResult = Pass | Fail | Error
                  deriving (Eq, Show)

main :: IO ()
main = do
  inputs <- filter (\fp -> take 1 fp /= ".")
         <$> getDirectoryContents ("test" </> "cases")
  args <- getArgs
  let regen = "--regenerate" `elem` args
  results <- forM inputs (runTest regen)
  let numfailures = length $ filter (== Fail) results
  let numerrors = length $ filter (== Error) results
  exitWith $ if numfailures == 0 && numerrors == 0
                then ExitSuccess
                else ExitFailure $ numfailures + numerrors

runTest :: Bool -> FilePath -> IO TestResult
runTest regen inpFile = do
  putStrLn $ "Testing with " ++ inpFile
  let casesdir = "test" </> "cases"
  let expecteddir = "test" </> "expected"
  code <- readFile (casesdir </> inpFile)
  let lang = drop 1 $ takeExtension inpFile
  syntax <- case lookupSyntax lang syntaxMap of
                 Just s  -> return s
                 Nothing -> fail $
                    "Could not find syntax definition for " ++ lang
  actual <- case tokenize syntaxMap syntax code of
                 Left e -> fail e
                 Right ls -> return $ renderHtml $
                                formatHtmlBlock defaultFormatOpts ls
  when regen $
    writeFile (expecteddir </> inpFile <.> "html") actual
  expectedString <- readFile (expecteddir </> inpFile <.> "html")
  if expectedString == actual
     then do
       putStrLn $ "[PASSED] " ++ inpFile
       return Pass
     else do
       putStrLn $ "[FAILED] " ++ inpFile
       putStrLn $ "--- " ++ (expecteddir </> inpFile <.> "html")
       putStrLn $ "+++ actual"
       printDiff expectedString actual
       return Fail

formatHtml toks =
  renderHtml $ H.head (metadata >> css) >> H.body (toHtml fragment)
  where css = H.style ! A.type_ "text/css" $ toHtml $ styleToCss pygments
        fragment = formatHtmlBlock opts toks
        metadata = H.meta H.! A.charset "utf-8"
        opts = defaultFormatOpts{ titleAttributes = True }

vividize :: Diff String -> String
vividize (Both s _) = "  " ++ s
vividize (First s)  = "- " ++ s
vividize (Second s) = "+ " ++ s

printDiff :: String -> String -> IO ()
printDiff expected actual = do
  mapM_ putStrLn $ map vividize $ getDiff (lines expected) (lines actual)
