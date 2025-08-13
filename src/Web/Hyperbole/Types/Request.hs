module Web.Hyperbole.Types.Request where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Web.Hyperbole.Data.URI (Path, Query)
import Network.HTTP.Types (Method)
import Data.Text (Text)
import Web.Hyperbole.Data.Cookie (Cookies)

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
