{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Effect.QueryData where

import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Word
import Text.Read (readMaybe)
import Web.HttpApiData


-- | Reimplement 'ToHttpApiData' based on Show
class ToQueryData a where
  toQueryData :: a -> Text
  default toQueryData :: (Show a) => a -> Text
  toQueryData = showQueryParam


-- | Reimplement 'FromHttpApiData' based on Read
class FromQueryData a where
  parseQueryData :: Text -> Either Text a
  default parseQueryData :: (Read a) => Text -> Either Text a
  parseQueryData = readQueryParam


instance ToQueryData Int where
  toQueryData = toQueryParam
instance FromQueryData Int where
  parseQueryData = parseQueryParam


instance ToQueryData Integer where
  toQueryData = toQueryParam
instance FromQueryData Integer where
  parseQueryData = parseQueryParam


instance ToQueryData Float where
  toQueryData = toQueryParam
instance FromQueryData Float where
  parseQueryData = parseQueryParam


instance ToQueryData Double where
  toQueryData = toQueryParam
instance FromQueryData Double where
  parseQueryData = parseQueryParam


instance ToQueryData Word where
  toQueryData = toQueryParam
instance FromQueryData Word where
  parseQueryData = parseQueryParam


instance ToQueryData Word8 where
  toQueryData = toQueryParam
instance FromQueryData Word8 where
  parseQueryData = parseQueryParam


instance ToQueryData Word16 where
  toQueryData = toQueryParam
instance FromQueryData Word16 where
  parseQueryData = parseQueryParam


instance ToQueryData Word32 where
  toQueryData = toQueryParam
instance FromQueryData Word32 where
  parseQueryData = parseQueryParam


instance ToQueryData Word64 where
  toQueryData = toQueryParam
instance FromQueryData Word64 where
  parseQueryData = parseQueryParam


instance ToQueryData Bool where
  toQueryData = toQueryParam
instance FromQueryData Bool where
  parseQueryData = parseQueryParam


instance ToQueryData Text where
  toQueryData = toQueryParam
instance FromQueryData Text where
  parseQueryData = parseQueryParam


instance ToQueryData Char where
  toQueryData = toQueryParam
instance FromQueryData Char where
  parseQueryData = parseQueryParam


instance ToQueryData UTCTime where
  toQueryData = toQueryParam
instance FromQueryData UTCTime where
  parseQueryData = parseQueryParam


instance (Show a) => ToQueryData [a] where
  toQueryData = showQueryParam
instance (Read a) => FromQueryData [a] where
  parseQueryData = readQueryParam


instance (ToQueryData a) => ToQueryData (Maybe a) where
  toQueryData Nothing = ""
  toQueryData (Just a) = toQueryData a
instance (Read a) => FromQueryData (Maybe a) where
  parseQueryData "" = pure Nothing
  parseQueryData t = readQueryParam t


instance (ToQueryData a, ToQueryData b) => ToQueryData (Either a b) where
  toQueryData (Left a) = toQueryData a
  toQueryData (Right b) = toQueryData b
instance (FromQueryData a, FromQueryData b) => FromQueryData (Either a b) where
  parseQueryData t =
    case parseQueryData @a t of
      Right a -> pure $ Left a
      Left _ -> do
        case parseQueryData @b t of
          Left _ -> Left $ "Could not parseQueryData Either: " <> t
          Right b -> pure $ Right b


-- | Encode a Show as a query param
showQueryParam :: (Show a) => a -> Text
showQueryParam a = toQueryParam $ show a


-- | Decode a Read as a query param
readQueryParam :: (Read a) => Text -> Either Text a
readQueryParam t = do
  str <- parseQueryParam t
  case readMaybe str of
    Nothing -> Left $ pack $ "Could not read query param: " <> str
    Just a -> pure a


parseQueryDatas :: (Traversable t, FromQueryData a) => t Text -> Either Text (t a)
parseQueryDatas = traverse parseQueryData
