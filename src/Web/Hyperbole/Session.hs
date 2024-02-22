{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Session where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as List
import Data.String.Conversions
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static
import Network.HTTP.Types (Query)
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import Web.Hyperbole.Effect
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.View


type Key = Text
type Value = Text
data Session :: Effect where
  Save :: Key -> Value -> Session m ()
  Load :: Key -> Session m Value
type instance DispatchOf Session = 'Dynamic


runSessionWai
  :: Request
  -> Eff (Session : es) a
  -> Eff es a
runSessionWai req = reinterpret id $ \_ -> \case
  -- we need to add to the response
  -- which means we need to be able to contribute to the response *gradually*
  -- which isn't currently possible
  Save k v -> _
  Load k -> _
