{-# LANGUAGE ScopedTypeVariables #-}

module Skylighting.Types (
                ContextName
              , SyntaxName
              , KeywordAttr(..)
              , Matcher(..)
              , Rule(..)
              , Context(..)
              , ContextSwitch(..)
              , Syntax(..)
              ) where

import Skylighting.Regex
import qualified Data.Map as Map
import qualified Data.Set as Set

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
  , rAttribute :: String
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
  -- , sItemDatas :: Map.Map String String -- TODO later, token
  } deriving (Show)

data Context = Context{
    cName  :: String
  , cRules :: [Rule]
  , cAttribute :: String
  , cLineEndContext :: [ContextSwitch]
  , cLineBeginContext :: [ContextSwitch]
  , cFallthrough :: Bool
  , cFallthroughContext :: [ContextSwitch]
  , cDynamic :: Bool
} deriving (Show)


