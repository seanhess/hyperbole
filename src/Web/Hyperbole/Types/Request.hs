module Web.Hyperbole.Types.Request where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Network.HTTP.Types (Method)
import Web.Hyperbole.Data.Cookie (Cookies)
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.URI (Path, Query)
import Web.Hyperbole.Types.Event (Event (..), TargetViewId)


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


data Request = Request
  { host :: Host
  , path :: Path
  , query :: Query
  , body :: BL.ByteString
  , method :: Method
  , cookies :: Cookies
  , event :: Maybe (Event TargetViewId Encoded Encoded)
  , requestId :: RequestId
  }
  deriving (Show)


newtype RequestId = RequestId Text
  deriving (Show)
