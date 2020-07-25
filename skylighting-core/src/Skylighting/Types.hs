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
              , Xterm256ColorCode(..)
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
import Data.Int
import Data.List (minimumBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import Safe (readMay)
import Skylighting.Regex
import qualified System.Console.ANSI.Types as ANSI
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
  | Keyword KeywordAttr !(WordSet Text)
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

-- | A syntax corresponds to a complete Kate syntax description.
-- The 'sShortname' field is derived from the filename.
data Syntax = Syntax{
    sName            :: !Text
  , sFilename        :: !String
  , sShortname       :: !Text
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

-- | A color (red/green/blue).
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

instance ToColor (ANSI.ColorIntensity, ANSI.Color) where
    toColor = flip lookup ansi16ColorList -- cannot actually fail

-- | Standard values taken from https://jonasjacek.github.io/colors/
ansi16ColorList :: [((ANSI.ColorIntensity, ANSI.Color), Color)]
ansi16ColorList = [ ((ANSI.Dull , ANSI.Black  ), RGB 0   0   0  )
                  , ((ANSI.Dull , ANSI.Red    ), RGB 128 0   0  )
                  , ((ANSI.Dull , ANSI.Green  ), RGB 0   128 0  )
                  , ((ANSI.Dull , ANSI.Yellow ), RGB 128 128 0  )
                  , ((ANSI.Dull , ANSI.Blue   ), RGB 0   0   128)
                  , ((ANSI.Dull , ANSI.Magenta), RGB 128 0   128)
                  , ((ANSI.Dull , ANSI.Cyan   ), RGB 0   128 128)
                  , ((ANSI.Dull , ANSI.White  ), RGB 192 192 192)
                  , ((ANSI.Vivid, ANSI.Black  ), RGB 128 128 128)
                  , ((ANSI.Vivid, ANSI.Red    ), RGB 255 0   0  )
                  , ((ANSI.Vivid, ANSI.Green  ), RGB 0   255 0  )
                  , ((ANSI.Vivid, ANSI.Yellow ), RGB 255 255 0  )
                  , ((ANSI.Vivid, ANSI.Blue   ), RGB 0   0   255)
                  , ((ANSI.Vivid, ANSI.Magenta), RGB 255 0   255)
                  , ((ANSI.Vivid, ANSI.Cyan   ), RGB 0   255 255)
                  , ((ANSI.Vivid, ANSI.White  ), RGB 255 255 255)
                  ]

newtype Xterm256ColorCode = Xterm256ColorCode { getXterm256ColorCode :: Word8 }
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

instance Binary Xterm256ColorCode

-- | Converted from https://jonasjacek.github.io/colors/data.json, then slightly rearranged
ansi256ColorList :: [(Xterm256ColorCode, Color)]
ansi256ColorList = [ (Xterm256ColorCode 232, RGB 8 8 8) -- grayscale colors
                   , (Xterm256ColorCode 233, RGB 18 18 18)
                   , (Xterm256ColorCode 234, RGB 28 28 28)
                   , (Xterm256ColorCode 235, RGB 38 38 38)
                   , (Xterm256ColorCode 236, RGB 48 48 48)
                   , (Xterm256ColorCode 237, RGB 58 58 58)
                   , (Xterm256ColorCode 238, RGB 68 68 68)
                   , (Xterm256ColorCode 239, RGB 78 78 78)
                   , (Xterm256ColorCode 240, RGB 88 88 88)
                   , (Xterm256ColorCode 241, RGB 98 98 98)
                   , (Xterm256ColorCode 242, RGB 108 108 108)
                   , (Xterm256ColorCode 243, RGB 118 118 118)
                   , (Xterm256ColorCode 244, RGB 128 128 128)
                   , (Xterm256ColorCode 245, RGB 138 138 138)
                   , (Xterm256ColorCode 246, RGB 148 148 148)
                   , (Xterm256ColorCode 247, RGB 158 158 158)
                   , (Xterm256ColorCode 248, RGB 168 168 168)
                   , (Xterm256ColorCode 249, RGB 178 178 178)
                   , (Xterm256ColorCode 250, RGB 188 188 188)
                   , (Xterm256ColorCode 251, RGB 198 198 198)
                   , (Xterm256ColorCode 252, RGB 208 208 208)
                   , (Xterm256ColorCode 253, RGB 218 218 218)
                   , (Xterm256ColorCode 254, RGB 228 228 228)
                   , (Xterm256ColorCode 255, RGB 238 238 238)
                   , (Xterm256ColorCode 16, RGB 0 0 0) -- RGB cube colors
                   , (Xterm256ColorCode 17, RGB 0 0 95)
                   , (Xterm256ColorCode 18, RGB 0 0 135)
                   , (Xterm256ColorCode 19, RGB 0 0 175)
                   , (Xterm256ColorCode 20, RGB 0 0 215)
                   , (Xterm256ColorCode 21, RGB 0 0 255)
                   , (Xterm256ColorCode 22, RGB 0 95 0)
                   , (Xterm256ColorCode 23, RGB 0 95 95)
                   , (Xterm256ColorCode 24, RGB 0 95 135)
                   , (Xterm256ColorCode 25, RGB 0 95 175)
                   , (Xterm256ColorCode 26, RGB 0 95 215)
                   , (Xterm256ColorCode 27, RGB 0 95 255)
                   , (Xterm256ColorCode 28, RGB 0 135 0)
                   , (Xterm256ColorCode 29, RGB 0 135 95)
                   , (Xterm256ColorCode 30, RGB 0 135 135)
                   , (Xterm256ColorCode 31, RGB 0 135 175)
                   , (Xterm256ColorCode 32, RGB 0 135 215)
                   , (Xterm256ColorCode 33, RGB 0 135 255)
                   , (Xterm256ColorCode 34, RGB 0 175 0)
                   , (Xterm256ColorCode 35, RGB 0 175 95)
                   , (Xterm256ColorCode 36, RGB 0 175 135)
                   , (Xterm256ColorCode 37, RGB 0 175 175)
                   , (Xterm256ColorCode 38, RGB 0 175 215)
                   , (Xterm256ColorCode 39, RGB 0 175 255)
                   , (Xterm256ColorCode 40, RGB 0 215 0)
                   , (Xterm256ColorCode 41, RGB 0 215 95)
                   , (Xterm256ColorCode 42, RGB 0 215 135)
                   , (Xterm256ColorCode 43, RGB 0 215 175)
                   , (Xterm256ColorCode 44, RGB 0 215 215)
                   , (Xterm256ColorCode 45, RGB 0 215 255)
                   , (Xterm256ColorCode 46, RGB 0 255 0)
                   , (Xterm256ColorCode 47, RGB 0 255 95)
                   , (Xterm256ColorCode 48, RGB 0 255 135)
                   , (Xterm256ColorCode 49, RGB 0 255 175)
                   , (Xterm256ColorCode 50, RGB 0 255 215)
                   , (Xterm256ColorCode 51, RGB 0 255 255)
                   , (Xterm256ColorCode 52, RGB 95 0 0)
                   , (Xterm256ColorCode 53, RGB 95 0 95)
                   , (Xterm256ColorCode 54, RGB 95 0 135)
                   , (Xterm256ColorCode 55, RGB 95 0 175)
                   , (Xterm256ColorCode 56, RGB 95 0 215)
                   , (Xterm256ColorCode 57, RGB 95 0 255)
                   , (Xterm256ColorCode 58, RGB 95 95 0)
                   , (Xterm256ColorCode 59, RGB 95 95 95)
                   , (Xterm256ColorCode 60, RGB 95 95 135)
                   , (Xterm256ColorCode 61, RGB 95 95 175)
                   , (Xterm256ColorCode 62, RGB 95 95 215)
                   , (Xterm256ColorCode 63, RGB 95 95 255)
                   , (Xterm256ColorCode 64, RGB 95 135 0)
                   , (Xterm256ColorCode 65, RGB 95 135 95)
                   , (Xterm256ColorCode 66, RGB 95 135 135)
                   , (Xterm256ColorCode 67, RGB 95 135 175)
                   , (Xterm256ColorCode 68, RGB 95 135 215)
                   , (Xterm256ColorCode 69, RGB 95 135 255)
                   , (Xterm256ColorCode 70, RGB 95 175 0)
                   , (Xterm256ColorCode 71, RGB 95 175 95)
                   , (Xterm256ColorCode 72, RGB 95 175 135)
                   , (Xterm256ColorCode 73, RGB 95 175 175)
                   , (Xterm256ColorCode 74, RGB 95 175 215)
                   , (Xterm256ColorCode 75, RGB 95 175 255)
                   , (Xterm256ColorCode 76, RGB 95 215 0)
                   , (Xterm256ColorCode 77, RGB 95 215 95)
                   , (Xterm256ColorCode 78, RGB 95 215 135)
                   , (Xterm256ColorCode 79, RGB 95 215 175)
                   , (Xterm256ColorCode 80, RGB 95 215 215)
                   , (Xterm256ColorCode 81, RGB 95 215 255)
                   , (Xterm256ColorCode 82, RGB 95 255 0)
                   , (Xterm256ColorCode 83, RGB 95 255 95)
                   , (Xterm256ColorCode 84, RGB 95 255 135)
                   , (Xterm256ColorCode 85, RGB 95 255 175)
                   , (Xterm256ColorCode 86, RGB 95 255 215)
                   , (Xterm256ColorCode 87, RGB 95 255 255)
                   , (Xterm256ColorCode 88, RGB 135 0 0)
                   , (Xterm256ColorCode 89, RGB 135 0 95)
                   , (Xterm256ColorCode 90, RGB 135 0 135)
                   , (Xterm256ColorCode 91, RGB 135 0 175)
                   , (Xterm256ColorCode 92, RGB 135 0 215)
                   , (Xterm256ColorCode 93, RGB 135 0 255)
                   , (Xterm256ColorCode 94, RGB 135 95 0)
                   , (Xterm256ColorCode 95, RGB 135 95 95)
                   , (Xterm256ColorCode 96, RGB 135 95 135)
                   , (Xterm256ColorCode 97, RGB 135 95 175)
                   , (Xterm256ColorCode 98, RGB 135 95 215)
                   , (Xterm256ColorCode 99, RGB 135 95 255)
                   , (Xterm256ColorCode 100, RGB 135 135 0)
                   , (Xterm256ColorCode 101, RGB 135 135 95)
                   , (Xterm256ColorCode 102, RGB 135 135 135)
                   , (Xterm256ColorCode 103, RGB 135 135 175)
                   , (Xterm256ColorCode 104, RGB 135 135 215)
                   , (Xterm256ColorCode 105, RGB 135 135 255)
                   , (Xterm256ColorCode 106, RGB 135 175 0)
                   , (Xterm256ColorCode 107, RGB 135 175 95)
                   , (Xterm256ColorCode 108, RGB 135 175 135)
                   , (Xterm256ColorCode 109, RGB 135 175 175)
                   , (Xterm256ColorCode 110, RGB 135 175 215)
                   , (Xterm256ColorCode 111, RGB 135 175 255)
                   , (Xterm256ColorCode 112, RGB 135 215 0)
                   , (Xterm256ColorCode 113, RGB 135 215 95)
                   , (Xterm256ColorCode 114, RGB 135 215 135)
                   , (Xterm256ColorCode 115, RGB 135 215 175)
                   , (Xterm256ColorCode 116, RGB 135 215 215)
                   , (Xterm256ColorCode 117, RGB 135 215 255)
                   , (Xterm256ColorCode 118, RGB 135 255 0)
                   , (Xterm256ColorCode 119, RGB 135 255 95)
                   , (Xterm256ColorCode 120, RGB 135 255 135)
                   , (Xterm256ColorCode 121, RGB 135 255 175)
                   , (Xterm256ColorCode 122, RGB 135 255 215)
                   , (Xterm256ColorCode 123, RGB 135 255 255)
                   , (Xterm256ColorCode 124, RGB 175 0 0)
                   , (Xterm256ColorCode 125, RGB 175 0 95)
                   , (Xterm256ColorCode 126, RGB 175 0 135)
                   , (Xterm256ColorCode 127, RGB 175 0 175)
                   , (Xterm256ColorCode 128, RGB 175 0 215)
                   , (Xterm256ColorCode 129, RGB 175 0 255)
                   , (Xterm256ColorCode 130, RGB 175 95 0)
                   , (Xterm256ColorCode 131, RGB 175 95 95)
                   , (Xterm256ColorCode 132, RGB 175 95 135)
                   , (Xterm256ColorCode 133, RGB 175 95 175)
                   , (Xterm256ColorCode 134, RGB 175 95 215)
                   , (Xterm256ColorCode 135, RGB 175 95 255)
                   , (Xterm256ColorCode 136, RGB 175 135 0)
                   , (Xterm256ColorCode 137, RGB 175 135 95)
                   , (Xterm256ColorCode 138, RGB 175 135 135)
                   , (Xterm256ColorCode 139, RGB 175 135 175)
                   , (Xterm256ColorCode 140, RGB 175 135 215)
                   , (Xterm256ColorCode 141, RGB 175 135 255)
                   , (Xterm256ColorCode 142, RGB 175 175 0)
                   , (Xterm256ColorCode 143, RGB 175 175 95)
                   , (Xterm256ColorCode 144, RGB 175 175 135)
                   , (Xterm256ColorCode 145, RGB 175 175 175)
                   , (Xterm256ColorCode 146, RGB 175 175 215)
                   , (Xterm256ColorCode 147, RGB 175 175 255)
                   , (Xterm256ColorCode 148, RGB 175 215 0)
                   , (Xterm256ColorCode 149, RGB 175 215 95)
                   , (Xterm256ColorCode 150, RGB 175 215 135)
                   , (Xterm256ColorCode 151, RGB 175 215 175)
                   , (Xterm256ColorCode 152, RGB 175 215 215)
                   , (Xterm256ColorCode 153, RGB 175 215 255)
                   , (Xterm256ColorCode 154, RGB 175 255 0)
                   , (Xterm256ColorCode 155, RGB 175 255 95)
                   , (Xterm256ColorCode 156, RGB 175 255 135)
                   , (Xterm256ColorCode 157, RGB 175 255 175)
                   , (Xterm256ColorCode 158, RGB 175 255 215)
                   , (Xterm256ColorCode 159, RGB 175 255 255)
                   , (Xterm256ColorCode 160, RGB 215 0 0)
                   , (Xterm256ColorCode 161, RGB 215 0 95)
                   , (Xterm256ColorCode 162, RGB 215 0 135)
                   , (Xterm256ColorCode 163, RGB 215 0 175)
                   , (Xterm256ColorCode 164, RGB 215 0 215)
                   , (Xterm256ColorCode 165, RGB 215 0 255)
                   , (Xterm256ColorCode 166, RGB 215 95 0)
                   , (Xterm256ColorCode 167, RGB 215 95 95)
                   , (Xterm256ColorCode 168, RGB 215 95 135)
                   , (Xterm256ColorCode 169, RGB 215 95 175)
                   , (Xterm256ColorCode 170, RGB 215 95 215)
                   , (Xterm256ColorCode 171, RGB 215 95 255)
                   , (Xterm256ColorCode 172, RGB 215 135 0)
                   , (Xterm256ColorCode 173, RGB 215 135 95)
                   , (Xterm256ColorCode 174, RGB 215 135 135)
                   , (Xterm256ColorCode 175, RGB 215 135 175)
                   , (Xterm256ColorCode 176, RGB 215 135 215)
                   , (Xterm256ColorCode 177, RGB 215 135 255)
                   , (Xterm256ColorCode 178, RGB 215 175 0)
                   , (Xterm256ColorCode 179, RGB 215 175 95)
                   , (Xterm256ColorCode 180, RGB 215 175 135)
                   , (Xterm256ColorCode 181, RGB 215 175 175)
                   , (Xterm256ColorCode 182, RGB 215 175 215)
                   , (Xterm256ColorCode 183, RGB 215 175 255)
                   , (Xterm256ColorCode 184, RGB 215 215 0)
                   , (Xterm256ColorCode 185, RGB 215 215 95)
                   , (Xterm256ColorCode 186, RGB 215 215 135)
                   , (Xterm256ColorCode 187, RGB 215 215 175)
                   , (Xterm256ColorCode 188, RGB 215 215 215)
                   , (Xterm256ColorCode 189, RGB 215 215 255)
                   , (Xterm256ColorCode 190, RGB 215 255 0)
                   , (Xterm256ColorCode 191, RGB 215 255 95)
                   , (Xterm256ColorCode 192, RGB 215 255 135)
                   , (Xterm256ColorCode 193, RGB 215 255 175)
                   , (Xterm256ColorCode 194, RGB 215 255 215)
                   , (Xterm256ColorCode 195, RGB 215 255 255)
                   , (Xterm256ColorCode 196, RGB 255 0 0)
                   , (Xterm256ColorCode 197, RGB 255 0 95)
                   , (Xterm256ColorCode 198, RGB 255 0 135)
                   , (Xterm256ColorCode 199, RGB 255 0 175)
                   , (Xterm256ColorCode 200, RGB 255 0 215)
                   , (Xterm256ColorCode 201, RGB 255 0 255)
                   , (Xterm256ColorCode 202, RGB 255 95 0)
                   , (Xterm256ColorCode 203, RGB 255 95 95)
                   , (Xterm256ColorCode 204, RGB 255 95 135)
                   , (Xterm256ColorCode 205, RGB 255 95 175)
                   , (Xterm256ColorCode 206, RGB 255 95 215)
                   , (Xterm256ColorCode 207, RGB 255 95 255)
                   , (Xterm256ColorCode 208, RGB 255 135 0)
                   , (Xterm256ColorCode 209, RGB 255 135 95)
                   , (Xterm256ColorCode 210, RGB 255 135 135)
                   , (Xterm256ColorCode 211, RGB 255 135 175)
                   , (Xterm256ColorCode 212, RGB 255 135 215)
                   , (Xterm256ColorCode 213, RGB 255 135 255)
                   , (Xterm256ColorCode 214, RGB 255 175 0)
                   , (Xterm256ColorCode 215, RGB 255 175 95)
                   , (Xterm256ColorCode 216, RGB 255 175 135)
                   , (Xterm256ColorCode 217, RGB 255 175 175)
                   , (Xterm256ColorCode 218, RGB 255 175 215)
                   , (Xterm256ColorCode 219, RGB 255 175 255)
                   , (Xterm256ColorCode 220, RGB 255 215 0)
                   , (Xterm256ColorCode 221, RGB 255 215 95)
                   , (Xterm256ColorCode 222, RGB 255 215 135)
                   , (Xterm256ColorCode 223, RGB 255 215 175)
                   , (Xterm256ColorCode 224, RGB 255 215 215)
                   , (Xterm256ColorCode 225, RGB 255 215 255)
                   , (Xterm256ColorCode 226, RGB 255 255 0)
                   , (Xterm256ColorCode 227, RGB 255 255 95)
                   , (Xterm256ColorCode 228, RGB 255 255 135)
                   , (Xterm256ColorCode 229, RGB 255 255 175)
                   , (Xterm256ColorCode 230, RGB 255 255 215)
                   , (Xterm256ColorCode 231, RGB 255 255 255)
                   , (Xterm256ColorCode 0, RGB 0 0 0) -- “system” colors
                   , (Xterm256ColorCode 1, RGB 128 0 0)
                   , (Xterm256ColorCode 2, RGB 0 128 0)
                   , (Xterm256ColorCode 3, RGB 128 128 0)
                   , (Xterm256ColorCode 4, RGB 0 0 128)
                   , (Xterm256ColorCode 5, RGB 128 0 128)
                   , (Xterm256ColorCode 6, RGB 0 128 128)
                   , (Xterm256ColorCode 7, RGB 192 192 192)
                   , (Xterm256ColorCode 8, RGB 128 128 128)
                   , (Xterm256ColorCode 9, RGB 255 0 0)
                   , (Xterm256ColorCode 10, RGB 0 255 0)
                   , (Xterm256ColorCode 11, RGB 255 255 0)
                   , (Xterm256ColorCode 12, RGB 0 0 255)
                   , (Xterm256ColorCode 13, RGB 255 0 255)
                   , (Xterm256ColorCode 14, RGB 0 255 255)
                   , (Xterm256ColorCode 15, RGB 255 255 255)
                   ]

instance ToColor Xterm256ColorCode where
    toColor = flip lookup ansi256ColorList -- cannot actually fail

-- | JSON @"#1aff2b" corresponds to the color @RGB 0x1a 0xff 0x2b@.
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

-- | Warning: this conversion is extremely approximate!
instance FromColor (ANSI.ColorIntensity, ANSI.Color) where
    fromColor = findApproximateColor ansi16ColorList

-- | Warning: this conversion is noticeably approximate!
instance FromColor Xterm256ColorCode where
    -- Same algorithm as above
    fromColor = findApproximateColor ansi256ColorList

colorDistance :: Color -> Color -> Int16
colorDistance (RGB r1 g1 b1) (RGB r2 g2 b2) = abs (fromIntegral r1 - fromIntegral r2)
                                                + abs (fromIntegral g1 - fromIntegral g2)
                                                + abs (fromIntegral b1 - fromIntegral b2)

-- This is the most naïve possible nearest-neighbor search;
-- it could almost certainly be optimized, if its speed matters at all.
findApproximateColor :: [(a, Color)] -> Color -> a
findApproximateColor acs c = let ranked = map (\ac -> (ac, colorDistance c $ snd ac)) acs
                      in fst . fst $ minimumBy (comparing snd) ranked

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
