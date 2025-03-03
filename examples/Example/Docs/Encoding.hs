{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.Encoding where

import Data.Text (Text)
import Data.Text qualified as Text
import Web.Hyperbole
import Web.Hyperbole.Data.QueryData (ParamValue (..))

data Filters = Filters
  { active :: Bool
  , term :: Text
  }
  deriving (Generic, FromQuery, ToQuery)

data Tags = Tags [Text]

instance ToParam Tags where
  toParam (Tags ts) = ParamValue $ Text.intercalate "," ts

instance FromParam Tags where
  parseParam (ParamValue t) =
    pure $ Tags $ Text.splitOn "," t
