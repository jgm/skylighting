{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Skylighting.Regex (
                Regex
              , RegexException
              , RE(..)
              , compileRegex
              , matchRegex
              ) where

import Text.Printf
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE.ByteString
import Data.ByteString.UTF8 (fromString, toString)
import qualified Control.Exception as E
import Data.Text (Text)
import qualified Data.Text as Text

newtype RegexException = RegexException String
      deriving (Show, Generic)

instance E.Exception RegexException

data RE = RE{
    reString :: String
  , reCompiled :: Maybe Regex
  , reCaseSensitive :: Bool
}

instance Show RE where
  show re = "RE{ reString = " ++ show (reString re) ++
            ", reCompiled = " ++
            (case reCompiled re of
                  Nothing  -> "Nothing"
                  Just _   -> "Just (compileRegex " ++
                                show (reCaseSensitive re) ++
                                " " ++ show (reString re) ++ ")") ++
            ", reCaseSensitive = " ++ show (reCaseSensitive re) ++ "}"

compileRegex :: Bool -> String -> Regex
compileRegex caseSensitive regexpStr =
  let opts = compAnchored + compUTF8 +
               if caseSensitive then 0 else compCaseless
  in  case unsafePerformIO $ compile opts (execNotEmpty)
             (fromString ('.' : convertOctal regexpStr)) of
            Left (off,msg) -> E.throw $ RegexException $
                        "Error compiling regex: " ++ regexpStr ++
                        " at offset " ++ show off ++ "\n" ++ msg
            Right r -> r

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
                 _          -> E.throw $ RegexException $
                                   "Unable to read octal number: " ++ ds
       _  -> '\\':'o':'{': convertOctal zs
convertOctal (x:xs) = x : convertOctal xs

isOctalDigit :: Char -> Bool
isOctalDigit c = c >= '0' && c <= '7'

matchRegex :: Regex -> String -> (Maybe [String])
matchRegex r s = case unsafePerformIO (regexec r (fromString s)) of
                      Right (Just (_, mat, _ , capts)) ->
                                       Just $ map toString (mat : capts)
                      Right Nothing -> Nothing
                      Left (_rc, msg) -> E.throw $ RegexException msg
