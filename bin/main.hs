{-# LANGUAGE OverloadedStrings #-}

import Skylighting
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Data.Char (toLower)
import Control.Monad
import Text.Show.Pretty (ppShow)
import qualified Data.Map as Map
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Version (showVersion)
import Paths_skylighting (version)

data Flag = Sty String
          | Format String
          | Help
          | Fragment
          | List
          | NumberLines
          | Syn String
          | TitleAttributes
          | Trace
          | Version
          deriving (Eq, Show)

data HighlightFormat = FormatHtml
                     | FormatLaTeX
                     | FormatNative
                     deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['S'] ["style"] (ReqArg Sty "STYLE") "specify style"
  , Option ['F'] ["format"] (ReqArg Format "FORMAT")  "output format (html|latex)"
  , Option ['f'] ["fragment"] (NoArg Fragment)  "fragment, without document header"
  , Option ['h'] ["help"] (NoArg Help)   "show usage message"
  , Option ['l'] ["list"] (NoArg List)   "list available language syntaxes"
  , Option ['n'] ["number-lines"] (NoArg NumberLines)  "number lines"
  , Option ['s'] ["syntax"] (ReqArg Syn "SYNTAX")  "specify language syntax to use"
  , Option ['t'] ["title-attributes"] (NoArg TitleAttributes)  "include structure in title attributes"
  , Option ['T'] ["trace"] (NoArg Trace)  "trace tokenizer for debugging"
  , Option ['v'] ["version"] (NoArg Version)   "print version"
  ]

syntaxOf :: [FilePath] -> [Flag] -> IO Syntax
syntaxOf fps [] = case concatMap (syntaxesByFilename syntaxMap) fps of
                       (s:_) -> return s
                       _     -> err "No syntax specified: use --syntax."
syntaxOf _ (Syn lang : _) = do
  case lookupSyntax lang syntaxMap of
         Just s  -> return s
         Nothing -> err ("Could not find syntax definition for " ++ lang)
syntaxOf fps (_:xs) = syntaxOf fps xs

styleOf :: [Flag] -> IO Style
styleOf [] = return pygments
styleOf (Sty s : _) = case map toLower s of
                            "pygments"   -> return pygments
                            "espresso"   -> return espresso
                            "kate"       -> return kate
                            "tango"      -> return tango
                            "haddock"    -> return haddock
                            "monochrome" -> return monochrome
                            _            -> error $ "Unknown style: " ++ s  -- TODO
styleOf (_ : xs) = styleOf xs

formatOf :: [Flag] -> IO HighlightFormat
formatOf [] = return FormatHtml  -- default
formatOf (Format s : _) = case map toLower s of
                            "html"   -> return FormatHtml
                            "latex"  -> return FormatLaTeX
                            "native" -> return FormatNative
                            _        -> error $ "Unknown format: " ++ s
formatOf (_ : xs) = formatOf xs

filterNewlines :: String -> String
filterNewlines ('\r':'\n':xs) = '\n' : filterNewlines xs
filterNewlines ('\r':xs) = '\n' : filterNewlines xs
filterNewlines (x:xs) = x : filterNewlines xs
filterNewlines [] = []

err :: String -> IO a
err msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  (opts, fnames, errs) <- getArgs >>= return . getOpt Permute options
  prg <- getProgName
  let usageHeader = prg ++ " [options] [files...]"
  when (not (null errs)) $
     err (concat errs ++ usageInfo usageHeader options)
  when (List `elem` opts) $ do
     let printSyntaxNames s = putStrLn (printf "%s (%s)"
                                       (map toLower $ sShortname s)
                                       (sName s))
     mapM_ printSyntaxNames $ Map.elems syntaxMap
     exitWith ExitSuccess
  when (Help `elem` opts) $ do
     hPutStrLn stderr (usageInfo usageHeader options)
     exitWith ExitSuccess
  when (Version `elem` opts) $ do
     putStrLn (prg ++ " " ++ showVersion version)
     exitWith ExitSuccess
  code <- if null fnames
             then getContents >>= return . filterNewlines
             else mapM readFile fnames >>= return . filterNewlines . concat

  let highlightOpts = defaultFormatOpts{ titleAttributes = TitleAttributes `elem` opts
                                       , numberLines = NumberLines `elem` opts
                                       , lineAnchors = NumberLines `elem` opts
                                       }
  let fragment = Fragment `elem` opts
  let fname = case fnames of
                    []    -> ""
                    (x:_) -> x
  let trace = Trace `elem` opts

  syntax <- syntaxOf fnames opts
  style <- styleOf opts
  format <- formatOf opts

  let tokenize' = if trace
                     then tokenizeWithTrace syntaxMap
                     else tokenize syntaxMap

  sourceLines <- case tokenize' syntax code of
                      Left e -> err e
                      Right ls -> return ls

  case format of
       FormatHtml -> hlHtml fragment fname highlightOpts style sourceLines
       FormatLaTeX -> hlLaTeX fragment fname highlightOpts style sourceLines
       FormatNative -> putStrLn $ ppShow sourceLines

hlHtml :: Bool               -- ^ Fragment
      -> FilePath            -- ^ Filename
      -> FormatOptions
      -> Style
      -> [SourceLine]
      -> IO ()
hlHtml frag fname opts sty sourceLines =
 if frag
    then putStrLn $ renderHtml fragment
    else putStrLn $ renderHtml $ H.head (pageTitle >> metadata >> css) >> H.body (H.toHtml fragment)
  where fragment = formatHtmlBlock opts sourceLines
        css = H.style H.! A.type_ "text/css" $ H.toHtml $ styleToCss sty
        pageTitle = H.title $ H.toHtml fname
        metadata = do
          H.meta H.! A.charset "utf-8"
          H.meta H.! A.name "generator" H.! A.content "highlight-kate"

hlLaTeX :: Bool               -- ^ Fragment
        -> FilePath            -- ^ Filename
        -> FormatOptions
        -> Style
        -> [SourceLine]
        -> IO ()
hlLaTeX frag fname opts sty sourceLines =
 if frag
    then putStrLn fragment
    else putStrLn $ "\\documentclass{article}\n\\usepackage[margin=1in]{geometry}\n" ++
                    macros ++ pageTitle ++
                    "\n\\begin{document}\n\\maketitle\n" ++  fragment ++ "\n\\end{document}"
  where fragment = formatLaTeXBlock opts sourceLines
        macros = styleToLaTeX sty
        pageTitle = "\\title{" ++ fname ++ "}\n"

