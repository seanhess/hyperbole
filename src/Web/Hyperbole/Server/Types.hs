module Web.Hyperbole.Server.Types where

import Control.Exception (Exception)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Effectful
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response


-- | Low level effect mapping request/response to either HTTP or WebSockets
data Server :: Effect where
  LoadRequest :: Server m Request
  SendResponse :: Client -> Response -> Server m ()


type instance DispatchOf Server = 'Dynamic


data InternalServerError
  = InvalidCookie BS.ByteString Text
  deriving (Show, Exception)


data SocketError
  = InvalidMessage Text
  | InternalSocket InternalServerError
  deriving (Show, Exception)


data ContentType
  = ContentHtml
  | ContentText


newtype Metadata = Metadata [(BL.ByteString, Text)] deriving newtype (Semigroup, Monoid)
