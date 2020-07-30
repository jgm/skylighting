{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Regex.KDE.Regex
 ( Direction(..)
 , Regex(..)
 , isWordChar
 ) where

import Data.Char
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>), Semigroup)
#endif

data Direction = Forward | Backward
  deriving (Show, Eq)

data Regex =
  MatchAnyChar |
  MatchDynamic !Int |
  MatchChar (Char -> Bool) |
  MatchSome !Regex |
  MatchAlt !Regex !Regex |
  MatchConcat !Regex !Regex |
  MatchCapture !Int !Regex |
  MatchCaptured !Int |
  AssertWordBoundary |
  AssertBeginning |
  AssertEnd |
  AssertPositive !Direction !Regex |
  AssertNegative !Direction !Regex |
  MatchNull

instance Show Regex where
  show MatchAnyChar = "MatchAnyChar"
  show (MatchDynamic i) = "MatchDynamic " <> show i
  show (MatchChar _) = "(MatchChar <fn>)"
  show (MatchSome re) = "(MatchSome " <> show re <> ")"
  show (MatchAlt r1 r2) = "(MatchAlt " <> show r1 <> " " <> show r2 <> ")"
  show (MatchConcat r1 r2) = "(MatchConcat " <> show r1 <> " " <> show r2 <>
            ")"
  show (MatchCapture i re) = "(MatchCapture " <> show i <> " " <>
                show re <> ")"
  show (MatchCaptured n) = "(MatchCaptured " <> show n <> ")"
  show AssertWordBoundary = "AssertWordBoundary"
  show AssertBeginning = "AssertBeginning"
  show AssertEnd = "AssertEnd"
  show (AssertPositive dir re) = "(AssertPositive " <> show dir <> " " <>
                  show re <> ")"
  show (AssertNegative dir re) = "(AssertNegativeLookahead " <>
                  show dir <> " " <> show re <> ")"
  show MatchNull = "MatchNull"

instance Semigroup Regex where
  (<>) = MatchConcat

instance Monoid Regex where
  mempty = MatchNull
  mappend = (<>)

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || generalCategory c == ConnectorPunctuation

