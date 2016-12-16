{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, TypeSynonymInstances,
    FlexibleInstances #-}

module Skylighting.Types (
                ContextName
              , SyntaxName
              , KeywordAttr(..)
              , Matcher(..)
              , Rule(..)
              , Context(..)
              , ContextSwitch(..)
              , Syntax(..)
              , Token
              , TokenType(..)
              , SourceLine
              , TokenStyle(..)
              , defStyle
              , Color(..)
              , ToColor(..)
              , FromColor(..)
              , Style(..)
              , FormatOptions(..)
              , defaultFormatOpts
              ) where

import Skylighting.Regex
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word
import Text.Printf
import Data.Bits
import Data.Data (Data)
import Data.Typeable (Typeable)

type ContextName = String
type SyntaxName = String

data KeywordAttr =
  KeywordAttr  { keywordCaseSensitive   :: Bool
               , keywordDelims          :: Set.Set Char
               }

instance Show KeywordAttr where
  show k = "KeywordAttr{ keywordCaseSensitive = " ++
            show (keywordCaseSensitive k) ++
           ", keywordDelims = (Data.Set." ++ show (keywordDelims k) ++ ")}"

data Matcher =
    DetectChar Char
  | Detect2Chars Char Char
  | AnyChar [Char]
  | RangeDetect Char Char
  | StringDetect String
  | RegExpr RE
  | Keyword KeywordAttr [String]
  | Int
  | Float
  | HlCOct
  | HlCHex
  | HlCStringChar
  | HlCChar
  | LineContinue
  | IncludeRules (Maybe SyntaxName) ContextName
  | DetectSpaces
  | DetectIdentifier
  | IfFirstNonspace Rule
  | IfColumn Int Rule
  | WithChildren Rule [Rule]
  | Unimplemented String
  deriving (Show)

data ContextSwitch =
  Pop | Push ContextName
  deriving Show

data Rule = Rule{
    rMatcher :: Matcher
  , rAttribute :: TokenType
  , rIncludeAttribute :: Bool
  , rDynamic   :: Bool
  , rChildren  ::  [Rule]
  , rContextSwitch :: [ContextSwitch]
  } deriving (Show)

data Syntax = Syntax{
    sName     :: String
  , sContexts :: Map.Map String Context
  , sAuthor   :: String
  , sVersion  :: String
  , sLicense  :: String
  , sExtensions :: [String]
  , sStartingContext :: Context
  -- , sItemDatas :: Map.Map String String -- TODO later, token
  } deriving (Show)

data Context = Context{
    cName  :: String
  , cRules :: [Rule]
  , cAttribute :: TokenType
  , cLineEndContext :: [ContextSwitch]
  , cLineBeginContext :: [ContextSwitch]
  , cFallthrough :: Bool
  , cFallthroughContext :: [ContextSwitch]
  , cDynamic :: Bool
} deriving (Show)

-- | A pair consisting of a list of attributes and some text.
type Token = (TokenType, String)

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
               deriving (Read, Show, Eq, Enum, Data, Typeable)

-- | A line of source, list of labeled source items.
type SourceLine = [Token]

data TokenStyle = TokenStyle {
    tokenColor      :: Maybe Color
  , tokenBackground :: Maybe Color
  , tokenBold       :: Bool
  , tokenItalic     :: Bool
  , tokenUnderline  :: Bool
  } deriving (Show, Read, Data, Typeable)

defStyle :: TokenStyle
defStyle = TokenStyle {
    tokenColor      = Nothing
  , tokenBackground = Nothing
  , tokenBold       = False
  , tokenItalic     = False
  , tokenUnderline  = False
  }

data Color = RGB Word8 Word8 Word8 deriving (Show, Read, Data, Typeable)

class ToColor a where
  toColor :: a -> Maybe Color

instance ToColor String where
  toColor ['#',r1,r2,g1,g2,b1,b2] =
     case reads ['(','0','x',r1,r2,',','0','x',g1,g2,',','0','x',b1,b2,')'] of
           ((r,g,b),_) : _ -> Just $ RGB r g b
           _                                         -> Nothing
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

class FromColor a where
  fromColor :: Color -> a

instance FromColor String where
  fromColor (RGB r g b) = printf "#%02x%02x%02x" r g b

instance FromColor (Double, Double, Double) where
  fromColor (RGB r g b) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

instance FromColor (Word8, Word8, Word8) where
  fromColor (RGB r g b) = (r, g, b)

data Style = Style {
    tokenStyles               :: [(TokenType, TokenStyle)]
  , defaultColor              :: Maybe Color
  , backgroundColor           :: Maybe Color
  , lineNumberColor           :: Maybe Color
  , lineNumberBackgroundColor :: Maybe Color
  } deriving (Read, Show, Data, Typeable)

-- | Options for formatting source code.
data FormatOptions = FormatOptions{
         numberLines      :: Bool     -- ^ Number lines
       , startNumber      :: Int      -- ^ Number of first line
       , lineAnchors      :: Bool     -- ^ Anchors on each line number
       , titleAttributes  :: Bool     -- ^ Html titles with token types
       , codeClasses      :: [String] -- ^ Additional classes for Html code tag
       , containerClasses :: [String] -- ^ Additional classes for Html container tag
                                      --   (pre or table depending on numberLines)
       } deriving (Eq, Show, Read)

defaultFormatOpts :: FormatOptions
defaultFormatOpts = FormatOptions{
                      numberLines = False
                    , startNumber = 1
                    , lineAnchors = False
                    , titleAttributes = False
                    , codeClasses = []
                    , containerClasses = []
                    }
