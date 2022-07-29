{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- | Basic types for Skylighting.
module Skylighting.Types (
              -- * Syntax descriptions
                ContextName
              , KeywordAttr(..)
              , WordSet(..)
              , makeWordSet
              , inWordSet
              , ListItem(..)
              , Matcher(..)
              , Rule(..)
              , Context(..)
              , ContextSwitch(..)
              , Syntax(..)
              , SyntaxMap
              -- * Tokens
              , Token
              , TokenType(..)
              , SourceLine
              , LineNo(..)
              -- * Styles
              , TokenStyle(..)
              , defStyle
              , Color(..)
              , ToColor(..)
              , FromColor(..)
              , Style(..)
              , ANSIColorLevel(..)
              -- * Format options
              , FormatOptions(..)
              , defaultFormatOpts
              ) where

import Control.Monad (mplus)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Binary (Binary)
import Data.Bits
import Data.CaseInsensitive (FoldCase (..))
import Data.Colour.SRGB (Colour, sRGB24, toSRGB24)
import qualified Data.Colour.SRGB as Colour
import Data.Data (Data)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import Safe (readMay)
import Skylighting.Regex
import Text.Printf

-- | Full name of a context: the first member of the pair is the full
-- syntax name, the second the context name within that syntax.
type ContextName = (Text, Text)

-- | Attributes controlling how keywords are interpreted.
data KeywordAttr =
  KeywordAttr  { keywordCaseSensitive :: !Bool
               , keywordDelims        :: !(Set.Set Char)
               }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Binary KeywordAttr

-- | A set of "words," possibly case insensitive.
data WordSet a = CaseSensitiveWords !(Set.Set a)
               | CaseInsensitiveWords !(Set.Set a)
     deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Binary a => Binary (WordSet a)

-- | A set of words to match (either case-sensitive or case-insensitive).
makeWordSet :: (FoldCase a, Ord a) => Bool -> [a] -> WordSet a
makeWordSet True ws  = CaseSensitiveWords (Set.fromList ws)
makeWordSet False ws = CaseInsensitiveWords (Set.fromList $ map foldCase ws)

-- | Test for membership in a 'WordSet'.
inWordSet :: (FoldCase a, Ord a) => a -> WordSet a -> Bool
inWordSet w (CaseInsensitiveWords ws) = foldCase w `Set.member` ws
inWordSet w (CaseSensitiveWords ws)   = w `Set.member` ws

-- | Matchers correspond to the element types in a context.
data Matcher =
    DetectChar !Char
  | Detect2Chars !Char !Char
  | AnyChar !(Set.Set Char)
  | RangeDetect !Char !Char
  | StringDetect !Text
  | WordDetect !Text
  | RegExpr !RE
  | Keyword !KeywordAttr (Either Text (WordSet Text))
      -- Either Left list name (unresolved) or Right wordset (resolved)
  | Int
  | Float
  | HlCOct
  | HlCHex
  | HlCStringChar
  | HlCChar
  | LineContinue
  | IncludeRules !ContextName
  | DetectSpaces
  | DetectIdentifier
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Binary Matcher

-- | A context switch, either pops or pushes a context.
data ContextSwitch =
  Pop | Push !ContextName
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Binary ContextSwitch

-- | A rule corresponds to one of the elements of a Kate syntax
-- highlighting "context."
data Rule = Rule{
    rMatcher          :: !Matcher
  , rAttribute        :: !TokenType
  , rIncludeAttribute :: !Bool
  , rDynamic          :: !Bool
  , rCaseSensitive    :: !Bool
  , rChildren         :: ![Rule]
  , rLookahead        :: !Bool
  , rFirstNonspace    :: !Bool
  , rColumn           :: !(Maybe Int)
  , rContextSwitch    :: ![ContextSwitch]
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Binary Rule

-- | A list item is either just a textual value or an included list.
-- IncludeList (x,y) includes list y from syntax with full name x.
data ListItem = Item !Text | IncludeList !(Text, Text)
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance Binary ListItem

-- | A syntax corresponds to a complete Kate syntax description.
-- The 'sShortname' field is derived from the filename.
data Syntax = Syntax{
    sName            :: !Text
  , sFilename        :: !String
  , sShortname       :: !Text
  , sLists           :: !(Map.Map Text [ListItem])
  , sContexts        :: !(Map.Map Text Context)
  , sAuthor          :: !Text
  , sVersion         :: !Text
  , sLicense         :: !Text
  , sExtensions      :: ![String]
  , sStartingContext :: !Text
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Binary Syntax

-- | A map of syntaxes, keyed by full name.
type SyntaxMap = Map.Map Text Syntax

-- | A Context corresponds to a context element in a Kate
-- syntax description.
data Context = Context{
    cName               :: !Text
  , cSyntax             :: !Text
  , cRules              :: ![Rule]
  , cAttribute          :: !TokenType
  , cLineEmptyContext   :: ![ContextSwitch]
  , cLineEndContext     :: ![ContextSwitch]
  , cLineBeginContext   :: ![ContextSwitch]
  , cFallthrough        :: !Bool
  , cFallthroughContext :: ![ContextSwitch]
  , cDynamic            :: !Bool
} deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Binary Context

-- | A pair consisting of a list of attributes and some text.
type Token = (TokenType, Text)

-- | 'KeywordTok' corresponds to @dsKeyword@ in Kate syntax
-- descriptions, and so on.
data TokenType = KeywordTok
               | DataTypeTok
               | DecValTok
               | BaseNTok
               | FloatTok
               | ConstantTok
               | CharTok
               | SpecialCharTok
               | StringTok
               | VerbatimStringTok
               | SpecialStringTok
               | ImportTok
               | CommentTok
               | DocumentationTok
               | AnnotationTok
               | CommentVarTok
               | OtherTok
               | FunctionTok
               | VariableTok
               | ControlFlowTok
               | OperatorTok
               | BuiltInTok
               | ExtensionTok
               | PreprocessorTok
               | AttributeTok
               | RegionMarkerTok
               | InformationTok
               | WarningTok
               | AlertTok
               | ErrorTok
               | NormalTok
               deriving (Read, Show, Eq, Ord, Enum, Data, Typeable, Generic)

instance Binary TokenType

instance ToJSON TokenType where
  toEncoding = toEncoding . Text.stripSuffix "Tok" . Text.pack . show

instance ToJSONKey TokenType where
  toJSONKey = toJSONKeyText
    (fromMaybe "Unknown" . Text.stripSuffix "Tok" . Text.pack . show)

instance FromJSON TokenType where
  parseJSON (String t) =
     case readMay (Text.unpack t ++ "Tok") of
          Just tt -> return tt
          Nothing -> fail "Not a token type"
  parseJSON _ = mempty

-- | JSON @"Keyword"@ corresponds to 'KeywordTok', and so on.
instance FromJSONKey TokenType where
  fromJSONKey = FromJSONKeyTextParser (\t ->
    case readMay (Text.unpack t ++ "Tok") of
         Just tt -> return tt
         Nothing -> fail "Not a token type")

-- | A line of source: a list of labeled tokens.
type SourceLine = [Token]

-- | Line numbers
newtype LineNo = LineNo { lineNo :: Int } deriving (Show, Enum)

-- | A 'TokenStyle' determines how a token is to be rendered.
data TokenStyle = TokenStyle {
    tokenColor      :: !(Maybe Color)
  , tokenBackground :: !(Maybe Color)
  , tokenBold       :: !Bool
  , tokenItalic     :: !Bool
  , tokenUnderline  :: !Bool
  } deriving (Show, Read, Ord, Eq, Data, Typeable, Generic)

instance Binary TokenStyle

-- | The keywords used in KDE syntax
-- themes are used, e.g. @text-color@ for default token color.
instance FromJSON TokenStyle where
  parseJSON (Object v) = do
    tcolor <- v .:? "text-color"
    bg <- v .:? "background-color"
    tbold <- v .:? "bold" .!= False
    titalic <- v .:? "italic" .!= False
    tunderline <- v .:? "underline" .!= False
    return TokenStyle{
               tokenColor = tcolor
             , tokenBackground = bg
             , tokenBold = tbold
             , tokenItalic = titalic
             , tokenUnderline = tunderline }
  parseJSON _ = mempty
instance ToJSON TokenStyle where
  toJSON ts = object [ "text-color" .= tokenColor ts
                     , "background-color" .= tokenBackground ts
                     , "bold" .= tokenBold ts
                     , "italic" .= tokenItalic ts
                     , "underline" .= tokenUnderline ts ]

-- | Default style.
defStyle :: TokenStyle
defStyle = TokenStyle {
    tokenColor      = Nothing
  , tokenBackground = Nothing
  , tokenBold       = False
  , tokenItalic     = False
  , tokenUnderline  = False
  }

-- | A color (red, green, blue).
data Color = RGB Word8 Word8 Word8
  deriving (Show, Read, Ord, Eq, Data, Typeable, Generic)

instance Binary Color

-- | Things that can be converted to a color.
class ToColor a where
  toColor :: a -> Maybe Color

instance ToColor String where
  toColor ['#',r1,r2,g1,g2,b1,b2] =
     case reads ['(','0','x',r1,r2,',','0','x',g1,g2,',','0','x',b1,b2,')'] of
           ((r,g,b),_) : _ -> Just $ RGB r g b
           _               -> Nothing
  toColor _        = Nothing

instance ToColor Int where
  toColor x = toColor (fromIntegral x1 :: Word8,
                       fromIntegral x2 :: Word8,
                       fromIntegral x3 :: Word8)
    where x1 = (shiftR x 16) .&. 0xFF
          x2 = (shiftR x 8 ) .&. 0xFF
          x3 = x             .&. 0xFF

instance ToColor (Word8, Word8, Word8) where
  toColor (r,g,b) = Just $ RGB r g b

instance ToColor (Double, Double, Double) where
  toColor (r,g,b) | r >= 0 && g >= 0 && b >= 0 && r <= 1 && g <= 1 && b <= 1 =
          Just $ RGB (floor $ r * 255) (floor $ g * 255) (floor $ b * 255)
  toColor _ = Nothing

instance (RealFrac a, Floating a) => ToColor (Colour a) where
    toColor c = let (Colour.RGB r g b) = toSRGB24 c in toColor (r, g, b)

-- | JSON @"#1aff2b"@ corresponds to the color @RGB 0x1a 0xff 0x2b@.
instance FromJSON Color where
  parseJSON (String t) = maybe mempty return $ toColor (Text.unpack t)
  parseJSON _          = mempty

instance ToJSON Color where
  toJSON color = String (Text.pack (fromColor color :: String))

-- | Different representations of a 'Color'.
class FromColor a where
  fromColor :: Color -> a

instance FromColor String where
  fromColor (RGB r g b) = printf "#%02x%02x%02x" r g b

instance FromColor (Double, Double, Double) where
  fromColor (RGB r g b) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

instance FromColor (Word8, Word8, Word8) where
  fromColor (RGB r g b) = (r, g, b)

instance (Ord a, Floating a) => FromColor (Colour a) where
    fromColor (RGB r g b) = sRGB24 r g b

-- | A rendering style. This determines how each kind of token
-- is to be rendered, and sets a default color and background
-- color for normal tokens.  Line numbers can have a different
-- color and background color.
data Style = Style {
    tokenStyles               :: !(Map.Map TokenType TokenStyle)
  , defaultColor              :: !(Maybe Color)
  , backgroundColor           :: !(Maybe Color)
  , lineNumberColor           :: !(Maybe Color)
  , lineNumberBackgroundColor :: !(Maybe Color)
  } deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance Binary Style

-- | The FromJSON instance for 'Style' is designed so that
-- a KDE syntax theme (JSON) can be decoded directly as a
-- 'Style'.
instance FromJSON Style where
  parseJSON (Object v) = do
    (tokstyles :: Map.Map Text TokenStyle) <- v .: "text-styles"
    (editorColors :: Map.Map Text Color) <- v .:? "editor-colors" .!= mempty
    mbBackgroundColor <- v .:? "background-color"
    mbLineNumberColor <- v .:? "line-number-color"
    mbDefaultColor <- v .:? "text-color"
    mbLineNumberBackgroundColor <- v .:? "line-number-background-color"
    return Style{ defaultColor = mbDefaultColor `mplus`
                     (case Map.lookup "Normal" tokstyles of
                           Nothing -> Nothing
                           Just ts -> tokenColor ts)
                , backgroundColor = mbBackgroundColor `mplus`
                     Map.lookup "background-color" editorColors
                , lineNumberColor = mbLineNumberColor `mplus`
                     Map.lookup "line-numbers" editorColors
                , lineNumberBackgroundColor =
                     mbLineNumberBackgroundColor `mplus`
                       Map.lookup "background-color" editorColors
                , tokenStyles =
                     Map.mapKeys (\s -> maybe OtherTok id $
                                     readMay (Text.unpack s ++ "Tok")) tokstyles }
  parseJSON _ = mempty

instance ToJSON Style where
  toJSON s = object [ "text-styles" .= toJSON (tokenStyles s)
                    , "background-color" .= toJSON (backgroundColor s)
                    , "text-color" .= toJSON (defaultColor s)
                    , "line-number-color" .= toJSON (lineNumberColor s)
                    , "line-number-background-color" .=
                         toJSON (lineNumberBackgroundColor s)
                    ]

-- | The available levels of color complexity in ANSI terminal output.
data ANSIColorLevel = ANSI16Color   -- ^ 16-color mode
                    | ANSI256Color  -- ^ 256-color mode
                    | ANSITrueColor -- ^ True-color mode
       deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Binary ANSIColorLevel

-- | Options for formatting source code.
data FormatOptions = FormatOptions{
         numberLines      :: !Bool           -- ^ Number lines
       , startNumber      :: !Int            -- ^ Number of first line
       , lineAnchors      :: !Bool           -- ^ Anchors on each line number
       , titleAttributes  :: !Bool           -- ^ Html titles with token types
       , codeClasses      :: ![Text]         -- ^ Additional classes for Html code tag
       , containerClasses :: ![Text]         -- ^ Additional classes for Html container tag
       , lineIdPrefix     :: !Text           -- ^ Prefix for id attributes on lines
       , ansiColorLevel   :: !ANSIColorLevel -- ^ Level of ANSI color support to use
       } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Binary FormatOptions

-- | Default formatting options.
defaultFormatOpts :: FormatOptions
defaultFormatOpts = FormatOptions{
                      numberLines = False
                    , startNumber = 1
                    , lineAnchors = False
                    , titleAttributes = False
                    , codeClasses = []
                    , containerClasses = []
                    , lineIdPrefix = ""
                    , ansiColorLevel = ANSI16Color
                    }
