module Web.Hyperbole.Host (Host (..)) where

import Data.ByteString qualified as BS


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)
