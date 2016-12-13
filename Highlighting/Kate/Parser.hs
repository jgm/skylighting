{-# LANGUAGE CPP #-}

module Highlighting.Kate.Parser (
                Regex
              , RE(..)
              , ContextName
              , SyntaxName
              , Matcher(..)
              , Rule(..)
              , Context(..)
              , ContextSwitch(..)
              , Syntax(..)
              ) where

import qualified Data.Text as Text
import Data.Text (Text)
#ifdef _PCRE_LIGHT
import Text.Regex.PCRE.Light (Regex)
import Data.ByteString (ByteString)
#else
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE.ByteString (Regex)
#endif

data RE = DynamicRegex Text | CompiledRegex Text Regex

instance Show RE where
  show (DynamicRegex t) = "DynamicRegex " ++ show t
  show (CompiledRegex t _) = "CompiledRegex " ++ show t

type ContextName = Text
type SyntaxName = Text

data Matcher =
    DetectChar Char
  | Detect2Chars Char Char
  | AnyChar [Char]
  | RangeDetect Char Char
  | StringDetect Text
  | RegExpr RE
  | Keyword [Text]
  | Int
  | Float
  | HlCOct
  | HlCHex
  | HlCStringChar
  | HlCChar
  | LineContinue
  | IncludeRules SyntaxName ContextName
  | DetectSpaces
  | DetectIdentifier
  | IfFirstNonspace Rule
  | IfColumn Int Rule
  | WithChildren Rule [Rule]
  | Unimplemented
  deriving (Show)

data ContextSwitch =
  Pop | Push Context
  deriving Show

data Rule = Rule{
    rMatcher :: Matcher
  , rAttribute :: Text
  , rDynamic   :: Bool
  , rChildren  ::  [Rule]
  , rContextSwitch :: [ContextSwitch]
  } deriving (Show)

data Syntax = Syntax{
    sName     :: Text
  , sContexts :: [Context]
  -- TODO more stuff.
  } deriving (Show)

data Context = Context{
    cName  :: Text
  , cRules :: [Rule]
    -- TODO more stuff
} deriving (Show)

