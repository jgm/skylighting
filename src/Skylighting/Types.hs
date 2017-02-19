{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Skylighting.Types (
              -- * Syntax descriptions
                ContextName
              , KeywordAttr(..)
              , WordSet(..)
              , makeWordSet
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
              -- * Styles
              , TokenStyle(..)
              , defStyle
              , Color(..)
              , ToColor(..)
              , FromColor(..)
              , Style(..)
              -- * Format options
              , FormatOptions(..)
              , defaultFormatOpts
              ) where

import Data.Aeson
import Data.Bits
import Data.CaseInsensitive (CI, FoldCase, mk)
import Data.Data (Data)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Word
import Safe (readMay)
import Skylighting.Regex
import Text.Printf

-- | Full name of a context: the first member of the pair is the full
-- syntax name, the second the context name within that syntax.
type ContextName = (Text, Text)

data KeywordAttr =
  KeywordAttr  { keywordCaseSensitive :: Bool
               , keywordDelims        :: Set.Set Char
               }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data WordSet a = CaseSensitiveWords (Set.Set a)
               | CaseInsensitiveWords (Set.Set (CI a))
     deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- | A set of words to match (either case-sensitive or case-insensitive).
makeWordSet :: (FoldCase a, Ord a) => Bool -> [a] -> WordSet a
makeWordSet True ws  = CaseSensitiveWords (Set.fromList ws)
makeWordSet False ws = CaseInsensitiveWords (Set.map mk (Set.fromList ws))

data Matcher =
    DetectChar Char
  | Detect2Chars Char Char
  | AnyChar [Char]
  | RangeDetect Char Char
  | StringDetect Text
  | WordDetect Text
  | RegExpr RE
  | Keyword KeywordAttr (WordSet Text)
  | Int
  | Float
  | HlCOct
  | HlCHex
  | HlCStringChar
  | HlCChar
  | LineContinue
  | IncludeRules ContextName
  | DetectSpaces
  | DetectIdentifier
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data ContextSwitch =
  Pop | Push ContextName
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- | A rule corresponds to one of the elements of a Kate syntax
-- highlighting "context."
data Rule = Rule{
    rMatcher          :: Matcher
  , rAttribute        :: TokenType
  , rIncludeAttribute :: Bool
  , rDynamic          :: Bool
  , rCaseSensitive    :: Bool
  , rChildren         :: [Rule]
  , rLookahead        :: Bool
  , rFirstNonspace    :: Bool
  , rColumn           :: Maybe Int
  , rContextSwitch    :: [ContextSwitch]
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- | A syntax corresponds to a complete Kate syntax description.
-- The 'sShortname' field is derived from the filename.
data Syntax = Syntax{
    sName            :: Text
  , sFilename        :: String
  , sShortname       :: Text
  , sContexts        :: Map.Map Text Context
  , sAuthor          :: Text
  , sVersion         :: Text
  , sLicense         :: Text
  , sExtensions      :: [String]
  , sStartingContext :: Text
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- | A map of syntaxes, keyed by full name.
type SyntaxMap = Map.Map Text Syntax

-- | A Context corresponds to a context element in a Kate
-- syntax description.
data Context = Context{
    cName               :: Text
  , cSyntax             :: Text
  , cRules              :: [Rule]
  , cAttribute          :: TokenType
  , cLineEmptyContext   :: [ContextSwitch]
  , cLineEndContext     :: [ContextSwitch]
  , cLineBeginContext   :: [ContextSwitch]
  , cFallthrough        :: Bool
  , cFallthroughContext :: [ContextSwitch]
  , cDynamic            :: Bool
} deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

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

-- | JSON @"Keyword"@ corresponds to 'KeywordTok', and so on.
instance FromJSON TokenType where
  parseJSON (String t) =
    case readMay (Text.unpack t ++ "Tok") of
         Just tt -> return tt
         Nothing -> fail "Not a token type"
  parseJSON _ = mempty

-- | A line of source: a list of labeled tokens.
type SourceLine = [Token]

-- | A 'TokenStyle' determines how a token is to be rendered.
data TokenStyle = TokenStyle {
    tokenColor      :: Maybe Color
  , tokenBackground :: Maybe Color
  , tokenBold       :: Bool
  , tokenItalic     :: Bool
  , tokenUnderline  :: Bool
  } deriving (Show, Read, Ord, Eq, Data, Typeable, Generic)

-- | The keywords used in KDE syntax
-- themes are used, e.g. @text-color@ for default token color.
instance FromJSON TokenStyle where
  parseJSON (Object v) = do
    tcolor <- v .:? "text-color"
    tbold <- v .:? "bold" .!= False
    titalic <- v .:? "italic" .!= False
    tunderline <- v .:? "underline" .!= False
    return TokenStyle{
               tokenColor = tcolor
             , tokenBackground = Nothing
             , tokenBold = tbold
             , tokenItalic = titalic
             , tokenUnderline = tunderline }
  parseJSON _ = mempty

-- | Default style.
defStyle :: TokenStyle
defStyle = TokenStyle {
    tokenColor      = Nothing
  , tokenBackground = Nothing
  , tokenBold       = False
  , tokenItalic     = False
  , tokenUnderline  = False
  }

-- A color (red/green/blue).
data Color = RGB Word8 Word8 Word8
  deriving (Show, Read, Ord, Eq, Data, Typeable, Generic)

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

-- | JSON @"#1aff2b" corresponds to the color @RGB 0x1a 0xff 0x2b@.
instance FromJSON Color where
  parseJSON (String t) = maybe mempty return $ toColor (Text.unpack t)
  parseJSON _          = mempty

class FromColor a where
  fromColor :: Color -> a

instance FromColor String where
  fromColor (RGB r g b) = printf "#%02x%02x%02x" r g b

instance FromColor (Double, Double, Double) where
  fromColor (RGB r g b) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

instance FromColor (Word8, Word8, Word8) where
  fromColor (RGB r g b) = (r, g, b)

-- | A rendering style. This determines how each kind of token
-- is to be rendered, and sets a default color and background
-- color for normal tokens.  Line numbers can have a different
-- color and background color.
data Style = Style {
    tokenStyles               :: [(TokenType, TokenStyle)]
  , defaultColor              :: Maybe Color
  , backgroundColor           :: Maybe Color
  , lineNumberColor           :: Maybe Color
  , lineNumberBackgroundColor :: Maybe Color
  } deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

-- | The FromJSON instance for 'Style' is designed so that
-- a KDE syntax theme (JSON) can be decoded directly as a
-- 'Style'.
instance FromJSON Style where
  parseJSON (Object v) = do
    (tokstyles :: Map.Map Text TokenStyle) <- v .: "text-styles"
    (editorColors :: Map.Map Text Color) <- v .: "editor-colors"
    return Style{ defaultColor = case Map.lookup "Normal" tokstyles of
                                      Nothing -> Nothing
                                      Just ts -> tokenColor ts
                , backgroundColor = Map.lookup "background-color" editorColors
                , lineNumberColor = Map.lookup "line-numbers" editorColors
                , lineNumberBackgroundColor = Map.lookup "background-color"
                                                editorColors
                , tokenStyles = Map.toList $
                     Map.mapKeys (\s -> maybe OtherTok id $
                                     readMay (Text.unpack s ++ "Tok")) tokstyles }
  parseJSON _ = mempty

-- | Options for formatting source code.
data FormatOptions = FormatOptions{
         numberLines      :: Bool     -- ^ Number lines
       , startNumber      :: Int      -- ^ Number of first line
       , lineAnchors      :: Bool     -- ^ Anchors on each line number
       , titleAttributes  :: Bool     -- ^ Html titles with token types
       , codeClasses      :: [Text]   -- ^ Additional classes for Html code tag
       , containerClasses :: [Text]   -- ^ Additional classes for Html container tag
                                      --   (pre or table depending on numberLines)
       } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

defaultFormatOpts :: FormatOptions
defaultFormatOpts = FormatOptions{
                      numberLines = False
                    , startNumber = 1
                    , lineAnchors = False
                    , titleAttributes = False
                    , codeClasses = []
                    , containerClasses = []
                    }
