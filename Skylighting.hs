module Skylighting (
    languages
--  , languagesByExtension
--  , languagesByFullName
--  , languagesByFilename
--  , highlightAs
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
import Data.List (sort)

-- | List of supported languages.
languages :: [String]
languages = sort $ Map.keys syntaxMap

syntaxes :: [Syntax]
syntaxes = Map.elems syntaxMap

{-
-- | List of language extensions.
languageExtensions :: [(String, String)]
languageExtensions = [(sName s, sExtensions s) | s <- syntaxes]

-- | List of full names of languages.
languageFullNames :: [(String, String)]
languageFullNames = [(sName s, sFullName s) | s <- syntaxes]

languageShortNames :: [(String, String)]
languageShortNames =
  [(map toLower y, map toLower x) | (x, y) <- languageFullNames]

-- | Lookup canonical language name by full syntaxName (e.g. "C#" for "Cs").
languageByFullName :: String -> Maybe String
languageByFullName s = undefined -- lookup s languageShortNames

-- | Returns a list of languages appropriate for the given file extension.
languagesByExtension :: String -> [String]
languagesByExtension ('.':ext) = languagesByFilename ("*." ++ ext)
languagesByExtension ext       = languagesByFilename ("*." ++ ext)

-- | Returns a list of languages appropriate for the given filename.
languagesByFilename :: FilePath -> [String]
languagesByFilename fn = undefined -- [lang | (lang, globs) <- languageExtensions, matchGlobs fn globs]
-}

-- | Highlight source code. The source language may be specified
-- by its canonical name (case-insensitive) or by a canonical
-- extension (if unique).
-- The parsers read the input lazily and parse line by line;
-- results are returned immediately.
highlightAs :: String         -- ^ Language syntax (e.g. "haskell") or extension (e.g. "hs").
            -> String         -- ^ Source code to highlight
            -> [SourceLine]   -- ^ List of highlighted source lines
highlightAs lang = undefined
{-
  let lang'  = map toLower lang
      lang'' = if lang' `elem` map (map toLower) languages
                  then lang'
                  else case languageByFullName lang' of
                            Just l -> l
                            Nothing ->
                              case languagesByExtension lang' of
                                    -- go by extension if unambiguous
                                    [l]  -> map toLower l
                                    _    -> lang'
-}
