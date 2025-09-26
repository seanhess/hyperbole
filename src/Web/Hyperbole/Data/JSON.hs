module Web.Hyperbole.Data.JSON (JSON (..), ToJSON, FromJSON) where

import Data.Aeson as A
import Data.String.Conversions (cs)
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.Param


-- JSON Encoded Data ------------------------------------

-- | This type always encodes data via JSON
newtype JSON a = JSON a


instance (ToJSON a) => ToEncoded (JSON a) where
  toEncoded (JSON a) = Encoded "" [jsonParam a]
instance (FromJSON a) => FromEncoded (JSON a) where
  parseEncoded (Encoded _ [ParamValue t]) = do
    JSON <$> A.eitherDecode (cs t)
  parseEncoded (Encoded _ prms) = do
    Left $ "Could not parse JSON Encoded, expected one param: " <> show prms
