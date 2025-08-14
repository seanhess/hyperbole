module Web.Hyperbole.Server.Types where

import Control.Exception (Exception)
import Data.ByteString qualified as BS
import Data.Text (Text)


data InternalServerError
  = InvalidCookie BS.ByteString Text
  deriving (Show, Exception)


data SocketError
  = InvalidMessage Text
  | InternalSocket InternalServerError
  deriving (Show, Exception)
