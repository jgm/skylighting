{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Skylighting.Parser (
                Regex
              , RE(..)
              , compileRegex
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
import Text.Regex.PCRE.ByteString (Regex, compCaseless, compUTF8, compAnchored, compile, execNotEmpty)
#endif
import qualified Data.Map as Map
import Data.ByteString.UTF8 (fromString)
import Text.Printf

data RE = RE{
    reString :: String
  , reCompiled :: Maybe Regex
  , reDynamic :: Bool
  , reCaseSensitive :: Bool
}

instance Show RE where
  show re = "RE{ reString = " ++ show (reString re) ++
            ", reCompiled = " ++
            (case reCompiled re of
                  Nothing  -> "Nothing"
                  Just r   -> "Just (compileRegex " ++
                                show (reCaseSensitive re) ++
                                " " ++ show (reString re) ++ ")") ++
            ", reDynamic = " ++ show (reDynamic re) ++
            ", reCaseSensitive = " ++ show (reCaseSensitive re) ++ "}"

compileRegex :: Bool -> String -> Regex
compileRegex caseSensitive regexpStr =
#ifdef _PCRE_LIGHT
  let opts = [anchored, utf8] ++ [caseless | not caseSensitive]
  in  compile (fromString ('.' : convertOctal regexpStr)) opts
#else
  let opts = compAnchored + compUTF8 +
               if caseSensitive then 0 else compCaseless
  in  case unsafePerformIO $ compile opts (execNotEmpty)
             (fromString ('.' : convertOctal regexpStr)) of
            Left e  -> error $ "Error compiling regex: " ++ show regexpStr ++
                               "\n" ++ show e -- TODO handle better
            Right r -> r
#endif

-- convert octal escapes to the form pcre wants.  Note:
-- need at least pcre 8.34 for the form \o{dddd}.
-- So we prefer \ddd or \x{...}.
convertOctal :: String -> String
convertOctal [] = ""
convertOctal ('\\':'0':x:y:z:rest)
  | all isOctalDigit [x,y,z] = '\\':x:y:z: convertOctal rest
convertOctal ('\\':x:y:z:rest)
  | all isOctalDigit [x,y,z] ='\\':x:y:z: convertOctal rest
convertOctal ('\\':'o':'{':zs) =
  case break (=='}') zs of
       (ds, '}':rest) | all isOctalDigit ds && not (null ds) ->
            case reads ('0':'o':ds) of
                 ((n :: Int,[]):_) -> printf "\\x{%x}" n ++ convertOctal rest
                 _          -> error $ "Unable to read octal number: " ++ ds
       _  -> '\\':'o':'{': convertOctal zs
convertOctal (x:xs) = x : convertOctal xs

isOctalDigit :: Char -> Bool
isOctalDigit c = c >= '0' && c <= '7'

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
  | Unimplemented String
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
  , sContexts :: Map.Map String Context
  -- TODO more stuff.
  } deriving (Show)

data Context = Context{
    cName  :: String
  , cRules :: [Rule]
    -- TODO more stuff
} deriving (Show)

