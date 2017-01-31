{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.Aeson (decode)
import Data.Monoid ((<>))
import Data.Algorithm.Diff
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Skylighting
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit
import Text.Show.Pretty
import qualified Data.Map as Map
import System.Random

syntaxes :: [Syntax]
syntaxes = Map.elems defaultSyntaxMap

defConfig :: TokenizerConfig
defConfig = TokenizerConfig{ traceOutput = False
                           , syntaxMap = defaultSyntaxMap }

tokToText :: Token -> Text
tokToText (_, s) = s

main :: IO ()
main = do
  stdgen <- newStdGen
  let randomText = Text.unlines $ Text.chunksOf 31
                     $ Text.pack $ take 5000 $ randomRs ('\0','\127') stdgen
  inputs <- filter (\fp -> take 1 fp /= ".")
         <$> getDirectoryContents ("test" </> "cases")
  allcases <- mconcat <$>
               mapM (Text.readFile . (("test" </> "cases") </>)) inputs
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
    , testGroup "Skylighting.Regex" $
      [ testCase "convertOctalEscapes" $
            "a\\700b\\700c\\x{800}" @=?
              convertOctalEscapes "a\\700b\\0700c\\o{4000}"
      ]
    , testGroup "Skylighting" $
      [ testCase "syntaxesByFilename" $
            ["Perl"] @=?
              map sName (syntaxesByFilename defaultSyntaxMap "foo/bar.pl")
      ]
    , testGroup "Doesn't hang or drop text on a mixed syntax sample" $
        map (noDropTest allcases) syntaxes
    , testGroup "Doesn't hang or drop text on fuzz" $
        map (noDropTest randomText) syntaxes
    , testGroup "Regression tests" $
      let perl = maybe (error "could not find Perl syntax") id
                             (lookupSyntax "Perl" defaultSyntaxMap) in
      [ testCase "perl NUL case" $ Right
             [[(KeywordTok,"s\NUL")
              ,(OtherTok,"b")
              ,(KeywordTok,"\NUL")
              ,(StringTok,"c")
              ,(KeywordTok,"\NUL")]]
             @=? tokenize defConfig perl "s\0b\0c\0"
      , testCase "perl quoting case" $ Right
           [ [ ( KeywordTok , "my" )
              , ( NormalTok , " " )
              , ( DataTypeTok , "$foo" )
              , ( NormalTok , " = " )
              , ( KeywordTok , "q/" )
              , ( StringTok , "bar" )
              , ( KeywordTok , "/" )
              , ( NormalTok , ";" )
              ]
            , [ ( KeywordTok , "my" )
              , ( NormalTok , " " )
              , ( DataTypeTok , "$baz" )
              , ( NormalTok , " = " )
              , ( KeywordTok , "'" )
              , ( StringTok , "quux" )
              , ( KeywordTok , "'" )
              , ( NormalTok , ";" )
              ]
            ] @=? tokenize defConfig perl
                     "my $foo = q/bar/;\nmy $baz = 'quux';\n"
      ]
    ]

compareValues :: FilePath -> Text -> Text -> IO (Maybe String)
compareValues referenceFile expected actual =
   if expected == actual
      then return $ Nothing
      else return $ Just $ makeDiff referenceFile
                           (Text.lines expected) (Text.lines actual)

makeDiff :: FilePath -> [Text] -> [Text] -> String
makeDiff referenceFile expected actual = unlines $
  [ "--- " ++ referenceFile
  , "+++ actual" ] ++
  map (Text.unpack . vividize) (filter notBoth (getDiff expected actual))
    where notBoth (Both _ _ ) = False
          notBoth _           = True

noDropTest :: Text -> Syntax -> TestTree
noDropTest inp syntax = localOption (mkTimeout 6000000) $
  testCase (Text.unpack (sName syntax)) $
      case tokenize defConfig syntax inp of
           Right ts -> assertBool ("Text has been dropped:\n" ++ diffs)
                        (inplines == toklines)
                where inplines = Text.lines inp
                      toklines = map (mconcat . map tokToText) ts
                      diffs = makeDiff "expected" inplines toklines
           Left  e  -> assert ("Unexpected error: " ++ e)

tokenizerTest :: Bool -> FilePath -> TestTree
tokenizerTest regen inpFile = localOption (mkTimeout 6000000) $
  goldenTest testname getExpected getActual
      (compareValues referenceFile) updateGolden
  where testname = lang ++ " tokenizing of " ++ inpFile
        getExpected = Text.readFile referenceFile
        getActual = do
          code <- Text.readFile (casesdir </> inpFile)
          syntax <- case lookupSyntax (Text.pack lang) defaultSyntaxMap of
                         Just s  -> return s
                         Nothing -> fail $
                            "Could not find syntax definition for " ++ lang
          case tokenize defConfig syntax $! code of
                 Left e   -> fail e
                 Right ls -> return $ Text.pack $ ppShow ls ++ "\n"
        updateGolden = if regen
                          then Text.writeFile referenceFile
                          else \_ -> return ()
        expecteddir = "test" </> "expected"
        casesdir = "test" </> "cases"
        referenceFile = expecteddir </> inpFile <.> "native"
        lang = drop 1 $ takeExtension inpFile

vividize :: Diff Text -> Text
vividize (Both s _) = "  " <> s
vividize (First s)  = "- " <> s
vividize (Second s) = "+ " <> s
