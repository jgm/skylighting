{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Control.Exception as E
import Data.Aeson (decode, encode)
import Data.Algorithm.Diff
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Skylighting.Core
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Text.Show.Pretty

syntaxes :: [Syntax]
syntaxes = Map.elems defaultSyntaxMap

defConfig :: TokenizerConfig
defConfig = TokenizerConfig{ traceOutput = False
                           , syntaxMap = defaultSyntaxMap }

tokToText :: Token -> Text
tokToText (_, s) = s

main :: IO ()
main = do
  inputs <- filter (\fp -> take 1 fp /= ".")
         <$> getDirectoryContents ("test" </> "cases")
  allcases <- mapM (Text.readFile . (("test" </> "cases") </>)) inputs
  args <- getArgs
  let regen = "--accept" `elem` args
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
       , testCase "round trip style -> theme -> style" $
            Just kate @=? decode (encode kate)
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
        map (\syn -> testProperty (Text.unpack (sName syn)) (p_no_drop syn))
        syntaxes
    , testGroup "Regression tests" $
      let perl = maybe (error "could not find Perl syntax") id
                             (lookupSyntax "Perl" defaultSyntaxMap)
          cpp  = maybe (error "could not find CPP syntax") id
                             (lookupSyntax "cpp" defaultSyntaxMap) in
      [ testCase "perl NUL case" $ Right
             [[(KeywordTok,"s\NUL")
              ,(OtherTok,"b")
              ,(KeywordTok,"\NUL")
              ,(StringTok,"c")
              ,(KeywordTok,"\NUL")]]
             @=? tokenize defConfig perl "s\0b\0c\0"
      , testCase "perl backslash case 1" $ Right
          [ [ ( KeywordTok , "m\\" )
            , ( OtherTok , "'" ) ]
          ] @=? tokenize defConfig perl
                     "m\\'"
      , testCase "perl backslash case 2" $ Right
          [ [ ( KeywordTok , "m\\" )
            , ( OtherTok , "a" )
            , ( KeywordTok , "\\" ) ]
          ] @=? tokenize defConfig perl
                     "m\\a\\"
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
      , testCase "cpp floats" $ Right
           [ [ ( FloatTok , "0.1f" ) ]
           , [ ( FloatTok , "1.0f" ) ]
           , [ ( FloatTok , "-0.1f" ) ]
           , [ ( FloatTok , "-1.0F" ) ]
           , [ ( FloatTok , "-1.0L" ) ]
           , [ ( FloatTok , "1e3" ) ]
           , [ ( FloatTok , "-15e+3" ) ]
           , [ ( FloatTok , "0.f" ) ]
           , [ ( FloatTok , "1.F" ) ]
           , [ ( DecValTok , "1" )
             , ( NormalTok ,".E" )
             , ( DecValTok , "3" )
             ]
           ] @=? tokenize defConfig cpp
                     "0.1f\n1.0f\n-0.1f\n-1.0F\n-1.0L\n1e3\n-15e+3\n0.f\n1.F\n1.E3"
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

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink xs = Text.pack <$> shrink (Text.unpack xs)

p_no_drop :: Syntax -> Text -> Bool
p_no_drop syntax t =
  case tokenize defConfig syntax t of
       Right ts -> Text.lines t == map (mconcat . map tokToText) ts
       Left _   -> False

noDropTest :: [Text] -> Syntax -> TestTree
noDropTest inps syntax =
  localOption (mkTimeout 9000000)
  $ testCase (Text.unpack (sName syntax))
  $ mapM_ go inps
    where go inp =
            E.catch
              (case tokenize defConfig syntax inp of
                    Right ts -> assertBool ("Text has been dropped:\n" ++ diffs)
                                 (inplines == toklines)
                         where inplines = Text.lines inp
                               toklines = map (mconcat . map tokToText) ts
                               diffs = makeDiff "expected" inplines toklines
                    Left  e  ->
                      assertFailure ("Unexpected error: " ++ e ++ "\ninput = " ++ show inp))
              (\(e :: RegexException) ->
                assertFailure (show e ++ "\ninput = " ++ show inp))

tokenizerTest :: Bool -> FilePath -> TestTree
tokenizerTest regen inpFile = localOption (mkTimeout 9000000) $
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
