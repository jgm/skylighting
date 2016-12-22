{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skylighting.Regex (
                Regex
              , RegexException
              , RE(..)
              , compileRegex
              , matchRegex
              , convertOctal
              ) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.UTF8 (toString)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import Text.Regex.PCRE.ByteString

newtype RegexException = RegexException String
      deriving (Show, Generic)

instance E.Exception RegexException

data RE = RE{
    reString        :: BS.ByteString
  , reCompiled      :: Maybe Regex
  , reCaseSensitive :: Bool
}

instance Show RE where
  show re = "RE{ reString = " ++ show (reString re) ++
            ", reCompiled = " ++
            (case reCompiled re of
                  Nothing  -> "Nothing"
                  Just _   -> "Just (compileRegex " ++
                                show (reCaseSensitive re) ++
                                " " ++ show (reString re)
                                ++ ")") ++
            ", reCaseSensitive = " ++ show (reCaseSensitive re) ++ "}"

compileRegex :: Bool -> BS.ByteString -> Regex
compileRegex caseSensitive regexpStr =
  let opts = compAnchored + compUTF8 +
               if caseSensitive then 0 else compCaseless
  in  case unsafePerformIO $ compile opts (execNotEmpty)
             (BS.cons '.' regexpStr) of
            Left (off,msg) -> E.throw $ RegexException $
                        "Error compiling regex /" ++ toString regexpStr ++
                        "/ at offset " ++ show off ++ "\n" ++ msg
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

matchRegex :: Regex -> BS.ByteString -> (Maybe [BS.ByteString])
matchRegex r s = case unsafePerformIO (regexec r s) of
                      Right (Just (_, mat, _ , capts)) ->
                                       Just (mat : capts)
                      Right Nothing -> Nothing
                      Left (_rc, msg) -> E.throw $ RegexException msg
