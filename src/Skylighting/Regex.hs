{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skylighting.Regex (
                Regex
              , RegexException
              , RE(..)
              , compileRegex
              , matchRegex
              , convertOctalEscapes
              ) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.UTF8 (toString)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import Text.Regex.PCRE.ByteString
import Data.Data
import Data.Binary (Binary)

-- | An exception in compiling or executing a regex.
newtype RegexException = RegexException String
      deriving (Show, Typeable, Generic)

instance E.Exception RegexException

-- | A representation of a regular expression.
data RE = RE{
    reString        :: BS.ByteString
  , reCaseSensitive :: Bool
} deriving (Show, Read, Ord, Eq, Data, Typeable, Generic)

instance Binary RE

-- | Compile a PCRE regex.  If the first parameter is True, the regex is
-- case-sensitive, otherwise caseless.  The regex is compiled from
-- a bytestring interpreted as UTF-8.  If the regex cannot be compiled,
-- a 'RegexException' is thrown.
compileRegex :: Bool -> BS.ByteString -> Regex
compileRegex caseSensitive regexpStr =
  let opts = compAnchored + compUTF8 +
               if caseSensitive then 0 else compCaseless
  in  case unsafePerformIO $ compile opts (execNotEmpty) regexpStr of
            Left (off,msg) -> E.throw $ RegexException $
                        "Error compiling regex /" ++ toString regexpStr ++
                        "/ at offset " ++ show off ++ "\n" ++ msg
            Right r -> r

-- | Convert octal escapes to the form pcre wants.  Note:
-- need at least pcre 8.34 for the form \o{dddd}.
-- So we prefer \ddd or \x{...}.
convertOctalEscapes :: String -> String
convertOctalEscapes [] = ""
convertOctalEscapes ('\\':'0':x:y:z:rest)
  | all isOctalDigit [x,y,z] = '\\':x:y:z: convertOctalEscapes rest
convertOctalEscapes ('\\':x:y:z:rest)
  | all isOctalDigit [x,y,z] ='\\':x:y:z: convertOctalEscapes rest
convertOctalEscapes ('\\':'o':'{':zs) =
  case break (=='}') zs of
       (ds, '}':rest) | all isOctalDigit ds && not (null ds) ->
            case reads ('0':'o':ds) of
                 ((n :: Int,[]):_) ->
                     printf "\\x{%x}" n ++ convertOctalEscapes rest
                 _          -> E.throw $ RegexException $
                                   "Unable to read octal number: " ++ ds
       _  -> '\\':'o':'{': convertOctalEscapes zs
convertOctalEscapes (x:xs) = x : convertOctalEscapes xs

isOctalDigit :: Char -> Bool
isOctalDigit c = c >= '0' && c <= '7'

-- | Match a 'Regex' against a bytestring.  Returns 'Nothing' if
-- no match, otherwise 'Just' a nonempty list of bytestrings. The first
-- bytestring in the list is the match, the others the captures, if any.
-- If there are errors in executing the regex, a 'RegexException' is
-- thrown.
matchRegex :: Regex -> BS.ByteString -> Maybe [BS.ByteString]
matchRegex r s = case unsafePerformIO (regexec r s) of
                      Right (Just (_, mat, _ , capts)) ->
                                       Just (mat : capts)
                      Right Nothing -> Nothing
                      Left (_rc, msg) -> E.throw $ RegexException msg
