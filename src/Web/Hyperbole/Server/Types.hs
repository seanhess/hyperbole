module Web.Hyperbole.Server.Types where

import Control.Exception (Exception)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)


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
