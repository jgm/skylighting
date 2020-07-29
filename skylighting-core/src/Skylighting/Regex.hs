{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skylighting.Regex (
                Regex
              , RE(..)
              , compileRegex
              , matchRegex
              , testRegex
              , isWordChar
              ) where

import Data.Aeson
import Data.Binary (Binary)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import Data.Data
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Regex.KDE

-- | A representation of a regular expression.
data RE = RE{
    reString        :: BS.ByteString
  , reCaseSensitive :: Bool
} deriving (Show, Read, Ord, Eq, Data, Typeable, Generic)

instance Binary RE

instance ToJSON RE where
  toJSON re = object [ "reString"        .= encodeToText (reString re)
                     , "reCaseSensitive" .= reCaseSensitive re ]
instance FromJSON RE where
  parseJSON = withObject "RE" $ \v ->
    RE <$> ((v .: "reString") >>= decodeFromText)
       <*> v .: "reCaseSensitive"

-- functions to marshall bytestrings to text

encodeToText :: BS.ByteString -> Text.Text
encodeToText = TE.decodeUtf8 . Base64.encode

decodeFromText :: (Monad m, MonadFail m) => Text.Text -> m BS.ByteString
decodeFromText = either fail return . Base64.decode . TE.encodeUtf8
