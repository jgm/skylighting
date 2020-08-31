{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.Maybe
import Data.Aeson (decode, encode)
import Data.Algorithm.Diff
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.Exit (exitFailure)
import System.IO (hSetEncoding, utf8, openFile, IOMode(..))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Text.Show.Pretty
import GHC.IO.Encoding (setLocaleEncoding)
import Skylighting.Core

readTextFile :: FilePath -> IO Text
readTextFile fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  Text.hGetContents h

tokToText :: Token -> Text
tokToText (_, s) = s

xmlPath :: FilePath
xmlPath = "xml/"

main :: IO ()
main = do
  setLocaleEncoding utf8
  sMap <- do
      result <- loadSyntaxesFromDir xmlPath
      case result of
          Left e -> do
              putStrLn $ "Error loading syntax definitions from " <> xmlPath <> ": " <> e
              exitFailure
          Right m -> return m

  let syntaxes = Map.elems sMap
      defConfig = TokenizerConfig { traceOutput = False
                                  , syntaxMap = sMap
                                  }
  let getMatchers = map rMatcher .  concatMap cRules . sContexts
  let getRegexFromMatcher (RegExpr re) = Just $ reString re
      getRegexFromMatcher _ = Nothing
  let getRegexesFromSyntax = mapMaybe getRegexFromMatcher . getMatchers
  inputs <- filter (\fp -> take 1 fp /= ".")
         <$> getDirectoryContents ("test" </> "cases")
  allcases <- mapM (fmap (Text.take 240)
                    . readTextFile . (("test" </> "cases") </>)) inputs
  args <- getArgs
  let regen = "--accept" `elem` args
  defaultTheme <- BL.readFile ("test" </> "default.theme")
  defaultMain $ testGroup "skylighting tests" $
    [ testGroup "tokenizer tests" $
        map (tokenizerTest defConfig sMap regen) inputs
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
    , testGroup "Skylighting" $
      [ testCase "syntaxesByFilename" $
            ["Perl"] @=?
              map sName (syntaxesByFilename sMap "foo/bar.pl")
      ]
    , testGroup "Doesn't hang or drop text on a mixed syntax sample" $
        map (noDropTest defConfig allcases) syntaxes
    , testGroup "Doesn't hang or drop text on fuzz" $
        map (\syn -> testProperty (Text.unpack (sName syn)) (p_no_drop defConfig syn))
        syntaxes
    , testGroup "All regexes compile" $
        map
           (\syn -> testGroup ("syntax " <> sFilename syn)
            (map
              (\regex ->
                testCase ("regex " <>
                           (Text.unpack $ TE.decodeUtf8 regex) <> " in "
                           <> sFilename syn)
             $ assertBool "regex does not compile"
               $ case compileRegex True regex of
                         Right _ -> True
                         Left _  -> False) $ getRegexesFromSyntax syn))
        syntaxes
    , testGroup "Regex module" $ map regexTest regexTests
    , testGroup "Regression tests" $
      let perl = maybe (error "could not find Perl syntax") id
                             (lookupSyntax "Perl" sMap)
          cpp  = maybe (error "could not find CPP syntax") id
                             (lookupSyntax "cpp" sMap)
          c    = maybe (error "could not find C syntax") id
                             (lookupSyntax "c" sMap) in
      [ testCase "perl NUL case" $ Right
             [[(OtherTok,"s\NULb\NUL")
              ,(StringTok,"c")
              ,(OtherTok,"\NUL")]]
             @=? tokenize defConfig perl "s\0b\0c\0"
      , testCase "perl backslash case 1" $ Right
          [[(OtherTok,"m\\'")]]
            @=? tokenize defConfig perl
                     "m\\'"
      , testCase "perl backslash case 2" $ Right
          [[(OtherTok,"m\\a\\")]]
            @=? tokenize defConfig perl
                     "m\\a\\"
      , testCase "perl quoting case" $ Right
          [[(KeywordTok,"my")
           ,(NormalTok," ")
           ,(DataTypeTok,"$foo")
           ,(NormalTok," = ")
           ,(OtherTok,"q/")
           ,(SpecialStringTok,"bar")
           ,(OtherTok,"/")
           ,(NormalTok,";")]
          ,[(KeywordTok,"my")
           ,(NormalTok," ")
           ,(DataTypeTok,"$baz")
           ,(NormalTok," = ")
           ,(OtherTok,"'")
           ,(SpecialStringTok,"quux")
           ,(OtherTok,"'")
           ,(NormalTok,";")]]
             @=? tokenize defConfig perl
                     "my $foo = q/bar/;\nmy $baz = 'quux';\n"
      , testCase "cpp floats" $ Right
           [ [ (FloatTok,"0.1") , (BuiltInTok,"f")]
           , [ (FloatTok,"1.0") , (BuiltInTok,"f")]
           , [ (NormalTok,"-") , (FloatTok,"0.1") , (BuiltInTok,"f")]
           , [ (NormalTok,"-") , (FloatTok,"1.0") , (BuiltInTok,"F")]
           , [ (NormalTok,"-") , (FloatTok,"1.0") , (BuiltInTok,"L")]
           , [ (FloatTok,"1e3")]
           , [ (NormalTok,"-") , (FloatTok,"15e+3")]
           , [ (FloatTok,"0.") , (BuiltInTok,"f")]
           , [ (FloatTok,"1.") , (BuiltInTok,"F")]
           , [ (FloatTok,"1.E3")]
           ] @=? tokenize defConfig cpp
                     "0.1f\n1.0f\n-0.1f\n-1.0F\n-1.0L\n1e3\n-15e+3\n0.f\n1.F\n1.E3"
      , testCase "cpp identifier (#76)" $ Right
           [ [ (NormalTok,"ng_or") ]
           ] @=? tokenize defConfig cpp "ng_or"

      , testCase "c '\\0' (#82)" $ Right
           [ [ (CharTok,"'\\0'") ]
           ] @=? tokenize defConfig c "'\\0'"

      , testCase "c very long integer (#81)" $ Right
           [ [ (DecValTok, "1111111111111111111111") ]
           ] @=? tokenize defConfig c "1111111111111111111111"
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

p_no_drop :: TokenizerConfig -> Syntax -> Text -> Bool
p_no_drop cfg syntax t =
  case tokenize cfg syntax t of
       Right ts -> Text.lines t == map (mconcat . map tokToText) ts
       Left _   -> False

noDropTest :: TokenizerConfig -> [Text] -> Syntax -> TestTree
noDropTest cfg inps syntax =
  localOption (mkTimeout 25000000)
  $ testCase (Text.unpack (sName syntax))
  $ mapM_ go inps
    where go inp =
              case tokenize cfg syntax inp of
                    Right ts -> assertBool ("Text has been dropped:\n" ++ diffs)
                                 (inplines == toklines)
                         where inplines = Text.lines inp
                               toklines = map (mconcat . map tokToText) ts
                               diffs = makeDiff "expected" inplines toklines
                    Left  e  ->
                      assertFailure ("Unexpected error: " ++ e ++ "\ninput = " ++ show inp)

tokenizerTest :: TokenizerConfig -> SyntaxMap -> Bool -> FilePath -> TestTree
tokenizerTest cfg sMap regen inpFile = localOption (mkTimeout 25000000) $
  goldenTest testname getExpected getActual
      (compareValues referenceFile) updateGolden
  where testname = lang ++ " tokenizing of " ++ inpFile
        getExpected = readTextFile referenceFile
        getActual = do
          code <- readTextFile (casesdir </> inpFile)
          syntax <- case lookupSyntax (Text.pack lang) sMap of
                         Just s  -> return s
                         Nothing -> fail $
                            "Could not find syntax definition for " ++ lang
          case tokenize cfg syntax $! code of
                 Left e   -> fail e
                 Right ls -> return $ Text.pack $ ppShow ls ++ "\n"
        updateGolden = if regen
                          then Text.writeFile referenceFile
                          else \_ -> return ()
        expecteddir = "test" </> "expected"
        casesdir = "test" </> "cases"
        referenceFile = expecteddir </> inpFile <.> "native"
        lang = drop 1 $ takeExtension inpFile

regexTest :: (String, String, Maybe (String, [(Int,String)])) -> TestTree
regexTest (re, inp, expected) =
  testCase ("/" ++ re ++ "/ " ++ inp) $
    expected @=? testRegex True re inp

regexTests :: [(String, String, Maybe (String, [(Int,String)]))]
regexTests =
  [ (".", "aab", Just ("a", []))
  , ("ab", "aab", Nothing)
  , ("ab", "abb", Just ("ab", []))
  , ("a(b)", "abb", Just ("ab", [(1,"b")]))
  , ("a(b.)*", "abbbcb", Just ("abbbc", [(1,"bc")]))
  , ("a(?:b.)*", "abbbcb", Just ("abbbc", []))
  , ("a(?=b)", "abb", Just ("a", []))
  , ("a(?=b)", "acb", Nothing)
  , ("a(?!b)", "abb", Nothing)
  , ("a(?!b)", "acb", Just ("a", []))
  , ("a?b+", "bbb", Just ("bbb", []))
  , ("a?b+", "abbb", Just ("abbb", []))
  , ("a?b+", "ac", Nothing)
  , ("a*", "bbb", Just ("", []))
  , ("abc|ab$", "ab", Just ("ab", []))
  , ("abc|ab$", "abcd", Just ("abc", []))
  , ("abc|ab$", "abd", Nothing)
  , ("(?:ab)*|a.*", "abababa", Just ("abababa", []))
  , ("a[b-e]*", "abcdefg", Just ("abcde", []))
  , ("a[b-e\\n-]*", "abcde\nb-bcfg", Just ("abcde\nb-bc", []))
  , ("^\\s+\\S+\\s+$", "   abc  ", Just ("   abc  ", []))
  , ("\\$", "$$", Just ("$", []))
  , ("[\\z12bb]", "\x12bb", Just ("\x12bb", []))
  , ("\\bhello\\b|hell", "hello there", Just ("hello", []))
  , ("\\bhello\\b|hell", "hellothere", Just ("hell", []))
  , ("[[:space:]]{2,4}.", "  abc", Just ("  a", []))
  , ("[[:space:]]{2,4}.", " abc", Nothing)
  , ("[[:space:]]{2,4}.", "     abc", Just ("     ", []))
  , ("((..)\\+\\2)", "aa+aabb+bbbc+cb",
          Just ("aa+aa", [(1,"aa+aa"), (2,"aa")]))
  , ("(\\d+)/(\\d+) == \\{1}", "22/2 == 22",
         Just ("22/2 == 22", [(1,"22"), (2,"2")]))
  , ("([a-z]+){2}", "htabc", Just ("htabc", [(1,"c")]))
  , ("((.+)(.+)(.+))*", (replicate 400 'a'),
          Just (replicate 400 'a',
                 [(1, replicate 400 'a')
                 ,(2,replicate 398 'a')
                 ,(3,"a")
                 ,(4,"a")]))
  , ("a++a", "aaaaa", Nothing)
  , ("\\w+e", "aaaeeee", Just ("aaaeeee", []))
  -- , ("\\w+?e", "aaaeeee", Just ("aaae", []))
  -- , ("a+b??", "aaab", Just ("aaa", []))
  , ("\\([a-z]+(?R)*\\)", "(aa(b(c)(d)))", Just ("(aa(b(c)(d)))", []))
  ]


vividize :: Diff Text -> Text
vividize (Both s _) = "  " <> s
vividize (First s)  = "- " <> s
vividize (Second s) = "+ " <> s
