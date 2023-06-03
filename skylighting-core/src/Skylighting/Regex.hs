{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skylighting.Regex (
                Regex(..)
              , RE
              , pattern RE, reCaseSensitive, reString
              , compileRE
              , compileRegex
              , matchRegex
              , testRegex
              , isWordChar
              ) where

import Data.Aeson
import Data.Binary (Binary(..))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import Data.Data
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Regex.KDE

import Text.Read hiding (get)

-- | A representation of a regular expression.
data RE = RE'{
    _reString        :: BS.ByteString
  , _reCaseSensitive :: Bool
  , reCompiled'      :: Either String Regex
} deriving Typeable

-- We define a smart constructor which also holds the compiled regex, to avoid
-- recompiling each time we tokenize.

{-# COMPLETE RE #-}
pattern RE :: BS.ByteString -> Bool  -> RE
pattern RE {reString, reCaseSensitive} <- RE' reString reCaseSensitive _ where
  RE str caseSensitive = RE' str caseSensitive (compileRegex caseSensitive str)

-- Unfortunately this means we need to derive all the instances ourselves.

instance Show RE where
  showsPrec d (RE str caseSensitive) = showParen (d > 10) 
    $ showString "RE {reString = " 
    . showsPrec 11 str
    . showString ", reCaseSensitive = "
    . showsPrec 11 caseSensitive
    . showString "}"

instance Read RE where
  readPrec = parens . prec 10 $ do
    Ident "RE" <- lexP
    Punc "{" <- lexP
    Ident "reString" <- lexP
    Punc "=" <- lexP
    str <- readPrec
    Punc "," <- lexP
    Ident "reCaseSensitive" <- lexP
    Punc "=" <- lexP
    caseSensitive <- readPrec
    Punc "}" <- lexP
    pure (RE str caseSensitive)

toComparisonKey :: RE -> (BS.ByteString, Bool)
toComparisonKey (RE x y) = (x, y)

instance Eq RE where
  x == y = toComparisonKey x == toComparisonKey y

instance Ord RE where
  x `compare` y = toComparisonKey x `compare` toComparisonKey y

conRE :: Constr
conRE = mkConstr tyRE "RE" [] Prefix
tyRE :: DataType
tyRE   = mkDataType "Skylighting.Regex.RE" [conRE]

instance Data RE where
  gfoldl k z (RE s c) = z RE `k` s `k` c
  gunfold k z _ = k (k (z RE))
  toConstr _ = conRE
  dataTypeOf _ = tyRE

instance Binary RE where
  put (RE x y) = put x >> put y
  get = RE <$> get <*> get

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

compileRE :: RE -> Either String Regex
compileRE = reCompiled'
