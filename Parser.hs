{-# LANGUAGE CPP #-}

module Parser ( Regex
              , Dynamic
              , RE
              , ContextName
              , SyntaxName
              , Rule(..)
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

data Dynamic = Dynamic | NotDynamic deriving (Read, Show, Eq)
data RE = DynamicRegex Text | CompiledRegex Text Regex

instance Show RE where
  show (DynamicRegex t) = "DynamicRegex " ++ show t
  show (CompiledRegex t _) = "CompiledRegex " ++ show t

type ContextName = Text
type SyntaxName = Text

data Rule =
    DetectChar Dynamic Char
  | Detect2Chars Dynamic Char Char
  | AnyChar [Char]
  | RangeDetect Char Char
  | StringDetect Dynamic Text
  | RegExpr RE
  | Keyword [Text]
  | Int
  | Float
  | HlCOct
  | HlCHex
  | HlCStringChar
  | HlCChar
  | LineContinue
  | IncludeRules ContextName SyntaxName
  | DetectSpaces
  | DetectIdentifier
  | Unimplemented
  deriving (Show)

