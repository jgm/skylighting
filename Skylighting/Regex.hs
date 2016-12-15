{-# LANGUAGE ScopedTypeVariables #-}

module Skylighting.Regex (
                Regex
              , RE(..)
              , compileRegex
              ) where

import Text.Printf
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE.ByteString (Regex, compCaseless, compUTF8, compAnchored, compile, execNotEmpty)
import Data.ByteString.UTF8 (fromString)

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
                  Just _   -> "Just (compileRegex " ++
                                show (reCaseSensitive re) ++
                                " " ++ show (reString re) ++ ")") ++
            ", reDynamic = " ++ show (reDynamic re) ++
            ", reCaseSensitive = " ++ show (reCaseSensitive re) ++ "}"

compileRegex :: Bool -> String -> Regex
compileRegex caseSensitive regexpStr =
  let opts = compAnchored + compUTF8 +
               if caseSensitive then 0 else compCaseless
  in  case unsafePerformIO $ compile opts (execNotEmpty)
             (fromString ('.' : convertOctal regexpStr)) of
            Left e  -> error $ "Error compiling regex: " ++ show regexpStr ++
                               "\n" ++ show e -- TODO handle better
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
                 _          -> error $ "Unable to read octal number: " ++ ds
       _  -> '\\':'o':'{': convertOctal zs
convertOctal (x:xs) = x : convertOctal xs

isOctalDigit :: Char -> Bool
isOctalDigit c = c >= '0' && c <= '7'

