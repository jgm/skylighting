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
  where testname = "highlighting of " ++ inpFile ++ "(" ++ lang ++ ")"
        getExpected = readFile (expecteddir </> inpFile <.> "html")
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
                 Right ls -> return $ renderHtml $
                                formatHtmlBlock defaultFormatOpts{
                                  titleAttributes = True } ls
        updateGolden = if regen
                          then writeFile (expecteddir </> inpFile <.> "html")
                          else \_ -> return ()
        expecteddir = "test" </> "expected"
        casesdir = "test" </> "cases"
        lang = drop 1 $ takeExtension inpFile
        compareValues expected actual = do
           if expected == actual
              then return Nothing
              else return $ Just $ unlines $
                   [ "--- " ++ (expecteddir </> inpFile <.> "html")
                   , "+++ actual" ] ++
                   map vividize (getDiff (lines expected) (lines actual))

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
