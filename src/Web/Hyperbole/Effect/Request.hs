module Web.Hyperbole.Effect.Request where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Types hiding (Query)
import Web.View (Query, Segment)


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


data Request = Request
  { host :: Host
  , path :: [Segment]
  , query :: Query
  , body :: BL.ByteString
  , method :: Method
  , cookies :: [(BS.ByteString, BS.ByteString)]
  }
  deriving (Show)
