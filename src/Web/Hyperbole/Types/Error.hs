{-# LANGUAGE LambdaCase #-}
module Web.Hyperbole.Types.Error where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (Query, QueryItem)
import Web.Atomic
import Web.Hyperbole.Data.URI (URI)
import Web.Hyperbole.Types.Event
import Web.Hyperbole.View (View)

data SerializedError = SerializedError
  { message :: Text
  , body :: BL.ByteString
  }
  deriving (Show)




serverError :: Text -> SerializedError
serverError msg =
  SerializedError msg (renderLazyByteString errorView)
 where
  errorView = el ~ pad 10 . bg (HexColor "#e53935") . color (HexColor "#fff") $ text msg

