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
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>), Semigroup)
#endif
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
             $ case compileRegex True regex of
                 Right _ -> assertBool "regex does not compile" True
                 Left e -> assertFailure ("regex does not compile: " <> show e))
                         $ getRegexesFromSyntax syn))
        syntaxes
    , testGroup "Regex module" $ map regexTest regexTests
    , testGroup "Regex module compile errors" $
        map regexErrorTest regexErrorTests
    , testGroup "Regression tests" $
      let perl = maybe (error "could not find Perl syntax") id
                             (lookupSyntax "Perl" sMap)
          html = maybe (error "could not find HTML syntax") id
                             (lookupSyntax "html" sMap)
          cpp  = maybe (error "could not find CPP syntax") id
                             (lookupSyntax "cpp" sMap)
          bash  = maybe (error "could not find bash syntax") id
                             (lookupSyntax "bash" sMap)
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
           , [ (OperatorTok,"-") , (FloatTok,"0.1") , (BuiltInTok,"f")]
           , [ (OperatorTok,"-") , (FloatTok,"1.0") , (BuiltInTok,"F")]
           , [ (OperatorTok,"-") , (FloatTok,"1.0") , (BuiltInTok,"L")]
           , [ (FloatTok,"1e3")]
           , [ (OperatorTok,"-") , (FloatTok,"15e+3")]
           , [ (FloatTok,"0.") , (BuiltInTok,"f")]
           , [ (FloatTok,"1.") , (BuiltInTok,"F")]
           , [ (FloatTok,"1.E3")]
           ] @=? tokenize defConfig cpp
                     "0.1f\n1.0f\n-0.1f\n-1.0F\n-1.0L\n1e3\n-15e+3\n0.f\n1.F\n1.E3"
      , testCase "cpp identifier (#76)" $ Right
           [ [ (NormalTok,"ng_or") ]
           ] @=? tokenize defConfig cpp "ng_or"

      , testCase "c '\\0' (#82)" $ Right
           [ [ (CharTok,"'"),(SpecialCharTok,"\\0"),(CharTok,"'") ]
           ] @=? tokenize defConfig c "'\\0'"

      , testCase "c very long integer (#81)" $ Right
           [ [ (DecValTok, "1111111111111111111111") ]
           ] @=? tokenize defConfig c "1111111111111111111111"

      , testCase "Chinese characters in HTML (#110)" $ Right
          [ [ ( NormalTok , "\35797\65306" ) , ( DataTypeTok , "<" ) ,
              ( KeywordTok , "a" ) , ( DataTypeTok , ">" ) ]
          ] @=? tokenize defConfig html "试：<a>"

      , testCase "Bash closing brace (#119)" $ Right
          [ [ ( FunctionTok , "f()" )
            , ( NormalTok , " " )
            , ( KeywordTok , "{" ) ]
          , [ ( NormalTok , "    " )
            , ( BuiltInTok , "echo" )
            , ( NormalTok , " " )
            , ( OperatorTok , ">" )
            , ( NormalTok , " f" ) ]
          , [ ( KeywordTok , "}" ) ] ]
             @=? tokenize defConfig bash
                     "f() {\n    echo > f\n}\n"

      , testCase "C floating-point literal (#174)" $ Right
          [ [ ( DataTypeTok , "double")
            , ( NormalTok , " x " )
            , ( OperatorTok , "=" )
            , ( NormalTok , " " )
            , ( FloatTok , "0.5")
            , ( OperatorTok , ";" ) ] ]
             @=? tokenize defConfig c
                     "double x = 0.5;\n"

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
  localOption (mkTimeout 75000000)
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
tokenizerTest cfg sMap regen inpFile = localOption (mkTimeout 75000000) $
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
  , ("a{2}b", "aaab", Nothing)
  , ("a{2,}b", "aaab", Just ("aaab", []))
  , ("a{2,3}b", "aab", Just ("aab", []))
  , ("a{2,3}b", "aaab", Just ("aaab", []))
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
  , ("[\\x50-\\x51]*", "PQR", Just ("PQ", []))
  , ("[\\x{2019}]*", "\x2019PQR", Just ("\x2019", []))
  , ("(?:ab)*|a.*", "abababa", Just ("ababab", []))
    -- leftmost-first: first alternative matches, so second is never tried
  , ("a[b-e]*", "abcdefg", Just ("abcde", []))
  , ("a[b-e\\n-]*", "abcde\nb-bcfg", Just ("abcde\nb-bc", []))
  , ("^\\s+\\S+\\s+$", "   abc  ", Just ("   abc  ", []))
  , ("\\$", "$$", Just ("$", []))
  , ("[\\z12bb]", "\x12bb", Just ("\x12bb", []))
  , ("[\\p{Lu}\\p{Ll}]*", "Σφa1B", Just ("Σφa", []))
  , ("\\bhello\\b|hell", "hello there", Just ("hello", []))
  , ("\\bhello\\b|hell", "hellothere", Just ("hell", []))
  , ("u\\b", "ue", Nothing)
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
  , ("\\w+?e", "aaaeeee", Just ("aaae", []))
  , ("a+b??", "aaab", Just ("aaa", []))
  , ("\\([a-z]+(?R)*\\)", "(aa(b(c)(d)))", Just ("(aa(b(c)(d)))", []))
  , ("a{}", "aaa", Nothing)
  , ("a{}", "a{}", Just ("a{}", []))
  , ("a{3", "a{3", Just ("a{3", []))
  , ("(?|(abc)|(def))", "abc", Just ("abc", [(1,"abc")]))
  , ("(?|(abc)|(def))", "def", Just ("def", [(1,"def")]))
  , ("(?:(abc)|(def))", "def", Just ("def", [(2,"def")]))
  , ("((?i:infinity|e|pi)|NaN)", "InfInity",
      Just ("InfInity", [(1,"InfInity")]))
--  , ("(?im)foo", "Foo", Just ("Foo", []))
  , ("d(?=(bc)|(ef))", "def", Just ("d", [(2,"ef")]))
  , ("([bcd])([efg])(?2)(?1)", "befd", Just ("befd", [(1,"b"),(2,"e")]))
  , ("([abc](?1)*)", "abcd", Just ("abc", [(1,"abc")]))
  , ("(x(?1)*)", "xxxxy", Just ("xxxx", [(1,"xxxx")]))
  , ("a|\\((?0)\\)", "(((a)))", Just ("(((a)))", []))
  , ("([abc](x(?1))*)", "axbxcc", Just ("axbxc", [(1,"axbxc"),(2,"xbxc")]))
    -- group 2's last iteration is "xbxc": the recursion (?1) inside it
    -- matches "bxc", and inner iterations' captures are overwritten
  , ("[\\p{Nd}]", "33", Just ("3", []))
  , ("\\p{N}", "33", Just ("3", []))
    -- {m,n} with m > n is a compile error (see regexErrorTests; it
    -- used to send the compiler into an infinite loop, and later was
    -- treated as a literal)
    -- lazy quantifiers in lookbehinds used to hang the matcher:
  , ("ab(?<=a+?b)c", "abc", Just ("abc", []))
  , ("ab(?<=a+?)c", "abc", Nothing)
    -- recursive subroutine calls that consume no input used to hang;
    -- now re-entering a subroutine at the same offset just fails:
  , ("x|(?R)", "x", Just ("x", []))
  , ("a|(?R)(?R)", "aa", Just ("a", []))
    -- leftmost-first: the first alternative succeeds on "a"
    -- backward matching (lookbehind, \b) after multibyte characters
    -- used to land inside a UTF-8 sequence:
  , ("\x00e9(?<=\x00e9)x", "\x00e9x", Just ("\x00e9x", []))
  , ("\x00e9\\bx", "\x00e9x", Nothing)
  , ("\x2019(?<=\x2019)x", "\x2019x", Just ("\x2019x", []))
  , ("\x00e9(?<!\x00e9)x", "\x00e9x", Nothing)
    -- (?i:...) is scoped to the group; it used to leak to the rest
    -- of the pattern:
  , ("(?i:a)b", "Ab", Just ("Ab", []))
  , ("(?i:a)b", "AB", Nothing)
  , ("x(?i:a(?-i:b)c)y", "xAbCy", Just ("xAbCy", []))
  , ("x(?i:a(?-i:b)c)y", "xABCy", Nothing)
    -- [[:graph:]] and [[:word:]] used to be unparseable (and graph
    -- meant "print"):
  , ("[[:graph:]]+", "ab cd", Just ("ab", []))
  , ("[[:word:]]+", "a_b-c", Just ("a_b", []))
  , ("[^[:graph:]]", " a", Just (" ", []))
  , ("[[:alpha:][:digit:]]+", "ab1 x", Just ("ab1", []))
    -- subroutine calls to groups nested inside other groups used to
    -- be silently ignored (matching the empty string):
  , ("((a)b)(?2)", "aba", Just ("aba", [(1,"ab"),(2,"a")]))
  , ("((a)b)(?2)", "abx", Nothing)
    -- character classes, escaped literals, and backreferences used
    -- to ignore case-insensitivity:
  , ("(?i:[abc]+)d", "aBcd", Just ("aBcd", []))
  , ("(?i:[a-z]+)!", "aBcD!", Just ("aBcD!", []))
  , ("(?i:[^a]+)", "xA", Just ("x", []))
  , ("(?i:\\x61+)", "aA", Just ("aA", []))
  , ("(?i:(ab)\\1)", "abAB", Just ("abAB", [(1,"ab")]))
  , ("(ab)\\1", "abAB", Nothing)
    -- {m,n} expansion is now linear in n; behavior is unchanged:
  , ("a{0,3}b", "aaab", Just ("aaab", []))
  , ("a{0,3}b", "aaaab", Nothing)
  , ("a{2,4}c", "aaaac", Just ("aaaac", []))
  , ("a{2,4}c", "aaaaac", Nothing)
  , ("[ab]{0,800}", replicate 800 'a', Just (replicate 800 'a', []))
    -- repeat counts over 65535 are compile errors (see
    -- regexErrorTests)
    -- an unmatched ] outside a character class is a literal, as in
    -- PCRE (used, e.g., by mermaid.xml and apparmor.xml):
  , ("a]b", "a]b", Just ("a]b", []))
  , ("\\d{1,3}]", "42]x", Just ("42]", []))
  , ("[ab]]", "b]", Just ("b]", []))
    -- a class is terminated by the first unescaped ] even if that
    -- yields a stray ] later (as in PCRE):
  , ("[^|{}[]", "a", Just ("a", []))
  , ("[^|{}[]", "[", Nothing)
    -- \0 takes up to two further octal digits, as in PCRE
    -- (\041 = '!', \042 = '"'); it is octal, not a backreference:
  , ("[\\041-\\043]", "\"", Just ("\"", []))
  , ("\\041", "!!", Just ("!", []))
  , ("a\\0b", "a\NULb", Just ("a\NULb", []))
  , ("[\\0]", "\NULx", Just ("\NUL", []))
    -- a third digit is not consumed (\0101 is \b followed by 1):
  , ("\\0101", "\b1", Just ("\b1", []))
    -- \G asserts the position where the match attempt started;
    -- since our matches are anchored, a leading \G is always true
    -- and a \G after consuming input always fails:
  , ("\\G\\d{4}-\\d{2}", "2024-01x", Just ("2024-01", []))
  , ("a\\Gb", "ab", Nothing)
    -- \g1 and \g{1} are PCRE syntax for backreferences:
  , ("(ab)\\g1", "ababx", Just ("abab", [(1,"ab")]))
  , ("(a)(b)\\g2\\g1", "abba", Just ("abba", [(1,"a"),(2,"b")]))
  , ("(ab)c\\g{1}", "abcabx", Just ("abcab", [(1,"ab")]))
  , ("([_*]{1,2})x\\g1", "**x**", Just ("**x**", [(1,"**")]))
  , ("([_*]{1,2})x\\g1", "**x*", Nothing)
    -- inline modifiers without a colon, like (?i), apply from that
    -- point to the end of the enclosing group (as in PCRE):
  , ("(?i)ab", "AB", Just ("AB", []))
  , ("A(?i)B", "aB", Nothing)
  , ("A(?i)B", "Ab", Just ("Ab", []))
  , ("x(?:(?i)a)Y", "xAY", Just ("xAY", []))
  , ("x(?:(?i)a)Y", "xAy", Nothing)
  , ("(a(?i)b|c)", "C", Just ("C", [(1,"C")]))
  , ("(?i)A(?-i)B", "aB", Just ("aB", []))
  , ("(?i)A(?-i)B", "Ab", Nothing)
    -- \h and \H match horizontal whitespace (and its complement):
  , ("a\\hb", "a b", Just ("a b", []))
  , ("a\\hb", "a\tb", Just ("a\tb", []))
  , ("a\\hb", "a\xa0\&b", Just ("a\xa0\&b", []))
  , ("\\h", "\x180e", Just ("\x180e", []))
  , ("a\\hb", "a\nb", Nothing)
  , ("a\\Hb", "axb", Just ("axb", []))
  , ("a\\Hb", "a b", Nothing)
  , ("[\\h]", "\xa0", Just ("\xa0", []))
  , ("[^\\h]+", "ab cd", Just ("ab", []))
    -- \A asserts the start of the subject:
  , ("\\Aab", "abc", Just ("ab", []))
  , ("a\\Ab", "ab", Nothing)
    -- an empty first alternative matches the empty string:
  , ("(?:|abc)x", "x", Just ("x", []))
  , ("(?:|abc)x", "abcx", Just ("abcx", []))
  , ("(?:\\d\\d(?:|[DT]\\d\\d))y", "12y", Just ("12y", []))
  , ("(?:\\d\\d(?:|[DT]\\d\\d))y", "12T34y", Just ("12T34y", []))
  , ("(?<=|)z\\d", "z4", Just ("z4", []))
    -- inside a character class, \b means backspace:
  , ("[\\b]", "\b", Just ("\b", []))
  , ("[\\b+-]x", "\bx", Just ("\bx", []))
  , ("[\\b+-]x", "+x", Just ("+x", []))
  , ("[\\b+-]x", "bx", Nothing)
    -- atomic groups (?>...):
  , ("(?>ab|a)c", "abc", Just ("abc", []))
  , ("(?>ab|a)c", "ac", Just ("ac", []))
  , ("(?>a+)ab", "aaab", Nothing)
  , ("x(?>)y", "xy", Just ("xy", []))
  , ("(?>a|ab)c", "abc", Nothing)
  , ("(?>(a|ab)c)", "abc", Just ("abc", [(1,"ab")]))
    -- alternation is leftmost-first, not longest-match (as in PCRE):
  , ("a|ab", "ab", Just ("a", []))
  , ("(a|ab)c?", "abc", Just ("a", [(1,"a")]))
  , ("(?=(a|ab))", "ab", Just ("", [(1,"a")]))
    -- lazy quantifiers match as little as possible:
  , ("a+?b", "aaab", Just ("aaab", []))
  , ("a*?b", "aaab", Just ("aaab", []))
  , ("(a+?)ab", "aaab", Just ("aaab", [(1,"aa")]))
    -- subroutine calls to groups with multi-digit numbers:
  , ("(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)x(?12)", "abcdefghijklxl",
      Just ("abcdefghijklxl",
            [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e"),(6,"f"),(7,"g"),
             (8,"h"),(9,"i"),(10,"j"),(11,"k"),(12,"l")]))
  , ("(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)x(?12)", "abcdefghijklxa", Nothing)
    -- numbering after (?|...) resumes after the highest group number
    -- used in any alternative:
  , ("(?|(a)(b)|(c))(d)\\2", "abdb",
      Just ("abdb", [(1,"a"),(2,"b"),(3,"d")]))
  , ("(?|(a)(b)|(c))(d)\\2", "cdd", Nothing)
    -- a ] in first position in a character class is a literal, and
    -- may be the start of a range:
  , ("[]-a]+", "^_`", Just ("^_`", []))
  , ("[]-a]+", "b", Nothing)
  , ("[]-]+", "]-]", Just ("]-]", []))
  , ("[]-]+", "^", Nothing)
  , ("[^]-a]+", "bz!", Just ("bz!", []))
  , ("[^]-a]+", "^", Nothing)
  , ("[]a-]+", "a]-", Just ("a]-", []))
    -- \pL is short for \p{L}; \P is the complement of \p:
  , ("\\pL+", "ab\x3a3\&9", Just ("ab\x3a3", []))
  , ("\\pN", "9", Just ("9", []))
  , ("\\pN", "a", Nothing)
  , ("\\PL+", "9!", Just ("9!", []))
  , ("\\P{L}+", "9!a", Just ("9!", []))
  , ("\\p{^L}+", "9!a", Just ("9!", []))
  , ("[\\pL]+", "ab9", Just ("ab", []))
  , ("[\\PL]+", "9!a", Just ("9!", []))
  , ("[\\P{N}]+", "a!9", Just ("a!", []))
  , ("[^\\PL]+", "ab9", Just ("ab", []))
    -- {,n} is a quantifier (as in PCRE 10.43+), but {,} and {b} are
    -- literal:
  , ("a{,2}", "aaa", Just ("aa", []))
  , ("a{,}", "a{,}", Just ("a{,}", []))
  , ("a{b}", "a{b}", Just ("a{b}", []))
    -- (?s) and (?m) are accepted (and are no-ops on our single-line
    -- subjects):
  , ("(?s)a.b", "axb", Just ("axb", []))
  , ("(?m)^ab", "ab", Just ("ab", []))
  , ("(?ims)ab", "AB", Just ("AB", []))
  ]

-- these should fail to compile, as they do in PCRE ("quantifier does
-- not follow a repeatable item", "numbers out of order in {}
-- quantifier", "number too big in {} quantifier", or an unsupported
-- inline flag):
regexErrorTests :: [String]
regexErrorTests =
  [ "{2}"
  , "({2})"
  , "a|{2}"
  , "a{2}{3}"
  , "a+{2}"
  , "{,2}"
  , "a{3,1}"
  , "x{70000}"
  , "a{2,70000}"
  , "^{2}"
  , "^*a"
  , "a$*"
  , "\\b+a"
  , "(?x)a b"
  , "(?n)(a)b"
  , "(?u)a"
  ]

regexErrorTest :: String -> TestTree
regexErrorTest re =
  testCase ("/" ++ re ++ "/") $
    case compileRegex True (TE.encodeUtf8 (Text.pack re)) of
      Left _  -> return ()
      Right _ -> assertFailure "regex compiled, but an error was expected"


vividize :: Diff Text -> Text
vividize (Both s _) = "  " <> s
vividize (First s)  = "- " <> s
vividize (Second s) = "+ " <> s
