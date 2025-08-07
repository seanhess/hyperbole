module Web.Hyperbole.Effect.Server.Types where

import Control.Exception (Exception)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Effectful
import Network.HTTP.Types (Method, Query)
import Web.Hyperbole.Data.Cookie (Cookies)
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (Path)
import Web.Hyperbole.Effect.Server.Response


-- | Low level effect mapping request/response to either HTTP or WebSockets
data Server :: Effect where
  LoadRequest :: Server m Request
  SendResponse :: Client -> Response -> Server m ()


type instance DispatchOf Server = 'Dynamic


data Client = Client
  { requestId :: RequestId
  , session :: Cookies
  , query :: Maybe QueryData
  }


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


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


data Request = Request
  { host :: Host
  , path :: Path
  , query :: Query
  , body :: BL.ByteString
  , method :: Method
  , cookies :: Cookies
  , requestId :: RequestId
  }
  deriving (Show)


newtype RequestId = RequestId Text
  deriving (Show)
