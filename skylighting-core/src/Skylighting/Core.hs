module Skylighting.Core (
    lookupSyntax
  , syntaxByName
  , syntaxByShortName
  , syntaxesByExtension
  , syntaxesByFilename
  , module Skylighting.Types
  , module Skylighting.Tokenizer
  , module Skylighting.Parser
  , module Skylighting.Regex
  , module Skylighting.Styles
  , module Skylighting.Loader
  , module Skylighting.Format.ANSI
  , module Skylighting.Format.HTML
  , module Skylighting.Format.LaTeX
  ) where
import Control.Monad
import Data.List (tails)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Skylighting.Format.ANSI
import Skylighting.Format.HTML
import Skylighting.Format.LaTeX
import Skylighting.Loader
import Skylighting.Parser
import Skylighting.Regex
import Skylighting.Styles
import Skylighting.Tokenizer
import Skylighting.Types

-- | Returns a list of syntaxes appropriate for the given file extension.
syntaxesByExtension :: SyntaxMap -> String -> [Syntax]
syntaxesByExtension syntaxmap ('.':ext) =
  syntaxesByFilename syntaxmap ("*." ++ ext)
syntaxesByExtension syntaxmap ext =
  syntaxesByFilename syntaxmap ("*." ++ ext)

-- | Returns a list of syntaxes appropriate for the given filename.
syntaxesByFilename :: SyntaxMap -> String -> [Syntax]
syntaxesByFilename syntaxmap fn = [s | s <- Map.elems syntaxmap
                                , matchGlobs fn (sExtensions s)]

-- | Lookup a syntax by full name (case insensitive).
syntaxByName :: SyntaxMap -> Text -> Maybe Syntax
syntaxByName syntaxmap name =
  Map.lookup (Text.toLower name) (Map.mapKeys Text.toLower syntaxmap)

-- | Lookup a syntax by short name (case insensitive).
syntaxByShortName :: SyntaxMap -> Text -> Maybe Syntax
syntaxByShortName syntaxmap name = listToMaybe
  [s | s <- Map.elems syntaxmap
     , Text.toLower (sShortname s) == Text.toLower name ]

-- | Lookup syntax by (in order) full name (case insensitive),
-- short name (case insensitive), extension.
lookupSyntax :: Text -> SyntaxMap -> Maybe Syntax
lookupSyntax lang syntaxmap
  -- special cases:
  | lang == Text.pack "csharp" = lookupSyntax (Text.pack "cs") syntaxmap
  | otherwise =
    syntaxByName syntaxmap lang `mplus`
    syntaxByShortName syntaxmap lang `mplus`
    listToMaybe (syntaxesByExtension syntaxmap (Text.unpack lang))

-- | Match filename against a list of globs contained in a semicolon-separated
-- string.
matchGlobs :: String -> [String] -> Bool
matchGlobs fn globs = any (flip matchGlob fn) globs

-- | Match filename against a glob pattern with asterisks.
matchGlob :: String -> String -> Bool
matchGlob ('*':xs) fn   = any (matchGlob xs) (tails fn)
matchGlob (x:xs) (y:ys) = x == y && matchGlob xs ys
matchGlob "" ""         = True
matchGlob _ _           = False
