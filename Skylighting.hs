module Skylighting (
    languages
  , languageFullNames
  , syntaxByExtension
  , syntaxByFilename
  , syntaxByFullName
  , highlightAs
  , module Skylighting.Types
  , module Skylighting.Tokenizer
  , module Skylighting.Parser
  , module Skylighting.Regex
  , module Skylighting.Syntax
  , module Skylighting.Styles
  , module Skylighting.Format.HTML
  , module Skylighting.Format.LaTeX
  ) where
import Skylighting.Types
import Skylighting.Tokenizer
import Skylighting.Parser
import Skylighting.Regex
import Skylighting.Syntax
import Skylighting.Styles
import Skylighting.Format.HTML
import Skylighting.Format.LaTeX
import qualified Data.Map as Map
import Data.List (sort, tails)
import Data.Char (toLower)

-- | List of supported languages.
languages :: [String]
languages = sort $ Map.keys syntaxMap

syntaxes :: [Syntax]
syntaxes = Map.elems syntaxMap

lowercaseSyntaxMap :: Map.Map String Syntax
lowercaseSyntaxMap = Map.mapKeys (map toLower) syntaxMap

-- | Returns a list of languages appropriate for the given file extension.
syntaxByExtension :: String -> [Syntax]
syntaxByExtension ('.':ext) = syntaxByFilename ("*." ++ ext)
syntaxByExtension ext       = syntaxByFilename ("*." ++ ext)

-- | Returns a list of languages appropriate for the given filename.
syntaxByFilename :: String -> [Syntax]
syntaxByFilename fn = [s | s <- syntaxes
                         , matchGlobs fn (sExtensions s)]

-- | Lookup canonical language name by full syntaxName (e.g. "C#" for "Cs").
syntaxByFullName :: String -> [Syntax]
syntaxByFullName name = [s | s <- syntaxes, sFullName s == name]

-- | List of full names of languages.
languageFullNames :: [String]
languageFullNames = [sFullName s | s <- syntaxes]

-- | Highlight source code. The source language may be specified
-- by its canonical name (case-insensitive) or by a canonical
-- extension (if unique) or by the full name in the syntax description.
highlightAs :: String         -- ^ Language syntax (e.g. "haskell") or extension (e.g. "hs").
            -> String         -- ^ Source code to highlight
            -> Either String [SourceLine]   -- ^ List of highlighted source lines
highlightAs "csharp" = highlightAs "cs" -- special case
highlightAs lang =
  case Map.lookup (map toLower lang) lowercaseSyntaxMap of
         Just s  -> tokenize s
         Nothing ->
           case syntaxByFullName lang of
                (s:_) -> tokenize s
                [] -> case syntaxByExtension lang of
                           (s:_) -> tokenize s
                           []    -> \_ -> Left
                               ("Could not find syntax definition for " ++ lang)

-- | Match filename against a list of globs contained in a semicolon-separated
-- string.
matchGlobs :: String -> [String] -> Bool
matchGlobs fn globs = any (flip matchGlob fn) globs

-- | Match filename against a glob pattern with asterisks.
matchGlob :: String -> String -> Bool
matchGlob ('*':xs) fn = any (matchGlob xs) (tails fn)
matchGlob (x:xs) (y:ys) = x == y && matchGlob xs ys
matchGlob "" "" = True
matchGlob _ _   = False
