{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.Encoding where

import Data.Text (Text)
import Web.Hyperbole

data Filters = Filters
  { active :: Bool
  , term :: Text
  }
  deriving (Generic, Eq, FromQuery, ToQuery)
