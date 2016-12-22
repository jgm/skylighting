module Skylighting (
    lookupSyntax
  , syntaxesByShortName
  , syntaxesByExtension
  , syntaxesByFilename
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
import Control.Monad
import qualified Data.Map as Map
import Data.List (tails)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Data.Text (Text)

-- | Returns a list of languages appropriate for the given file extension.
syntaxesByExtension :: SyntaxMap -> String -> [Syntax]
syntaxesByExtension syntaxmap ('.':ext) =
  syntaxesByFilename syntaxmap ("*." ++ ext)
syntaxesByExtension syntaxmap ext =
  syntaxesByFilename syntaxmap ("*." ++ ext)

-- | Returns a list of languages appropriate for the given filename.
syntaxesByFilename :: SyntaxMap -> String -> [Syntax]
syntaxesByFilename syntaxmap fn = [s | s <- Map.elems syntaxmap
                                , matchGlobs fn (sExtensions s)]

-- | Lookup syntax by short name (case insensitive).
syntaxesByShortName :: SyntaxMap -> Text -> [Syntax]
syntaxesByShortName syntaxmap name = [s | s <- Map.elems syntaxmap
                                   , Text.toLower (sShortname s) ==
                                     Text.toLower name ]

-- | Lookup syntax by (in order) full name (case insensitive),
-- short name (case insensitive), extension.
lookupSyntax :: Text -> SyntaxMap -> Maybe Syntax
lookupSyntax lang syntaxmap
  -- special cases:
  | lang == Text.pack "csharp" = lookupSyntax (Text.pack "cs") syntaxmap
  | otherwise =
  Map.lookup (Text.toLower lang) (Map.mapKeys Text.toLower syntaxmap) `mplus`
    listToMaybe (syntaxesByShortName syntaxmap lang ++
                 syntaxesByExtension syntaxmap (Text.unpack lang))

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
