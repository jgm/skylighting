{-# LANGUAGE CPP #-}

module Skylighting.Parser (
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

#ifdef _PCRE_LIGHT
import Text.Regex.PCRE.Light (Regex)
import Data.ByteString (ByteString)
#else
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE.ByteString (Regex)
#endif

data RE = DynamicRegex String | CompiledRegex String Regex

instance Show RE where
  show (DynamicRegex t) = "DynamicRegex " ++ show t
  show (CompiledRegex t _) = "CompiledRegex " ++ show t

type ContextName = String
type SyntaxName = String

data Matcher =
    DetectChar Char
  | Detect2Chars Char Char
  | AnyChar [Char]
  | RangeDetect Char Char
  | StringDetect String
  | RegExpr RE
  | Keyword [String]
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
  , rAttribute :: String
  , rDynamic   :: Bool
  , rChildren  ::  [Rule]
  , rContextSwitch :: [ContextSwitch]
  } deriving (Show)

data Syntax = Syntax{
    sName     :: String
  , sContexts :: [Context]
  -- TODO more stuff.
  } deriving (Show)

data Context = Context{
    cName  :: String
  , cRules :: [Rule]
    -- TODO more stuff
} deriving (Show)

