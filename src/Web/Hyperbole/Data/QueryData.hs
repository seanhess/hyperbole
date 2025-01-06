{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Data.QueryData where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Word
import GHC.Generics
import Network.HTTP.Types (Query, renderQuery)
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)
import Web.HttpApiData
import Prelude hiding (lookup)


-- | Key-value store for query params and sessions
newtype QueryData = QueryData (Map Text Text)
  deriving (Show)
  deriving newtype (Monoid, Semigroup)


singleton :: (ToParam a) => Text -> a -> QueryData
singleton key a = QueryData $ M.singleton key (toParam a)


insert :: (ToParam a) => Text -> a -> QueryData -> QueryData
insert k a (QueryData m) =
  let val = toParam a
   in QueryData $ M.insert k val m


insertAll :: (ToQuery a) => a -> QueryData -> QueryData
insertAll a (QueryData m) =
  let QueryData kvs = toQuery a
   in QueryData $ M.union kvs m


delete :: Text -> QueryData -> QueryData
delete k (QueryData m) =
  QueryData $ M.delete k m


lookup :: (FromParam a) => Text -> QueryData -> Maybe a
lookup k (QueryData m) = do
  t <- M.lookup k m
  either (const Nothing) pure $ parseParam t


require :: (FromParam a) => Text -> QueryData -> Either Text a
require k qd = do
  case lookup k qd of
    Nothing -> Left $ "Missing Key: " <> k
    Just t -> parseParam t


filterKey :: (Text -> Bool) -> QueryData -> QueryData
filterKey p (QueryData m) =
  QueryData $ M.filterWithKey (\k _ -> p k) m


member :: Text -> QueryData -> Bool
member k (QueryData qd) = M.member k qd


elems :: QueryData -> [Text]
elems (QueryData m) = M.elems m


render :: QueryData -> ByteString
render (QueryData m) =
  -- urlEncode True
  renderQuery False (HTTP.toQuery $ M.toList m)


parse :: ByteString -> QueryData
parse =
  -- urlDecode True
  queryData . HTTP.parseQuery


queryData :: Query -> QueryData
queryData =
  QueryData . M.fromList . map (bimap cs value)
 where
  -- empty / missing values are encoded as empty strings
  value Nothing = ""
  value (Just v) = cs v


-- | Decode a type from a Query
class FromQuery a where
  parseQuery :: QueryData -> Either Text a
  default parseQuery :: (Generic a, GFromQuery (Rep a)) => QueryData -> Either Text a
  parseQuery q = to <$> gParseQuery q


instance FromQuery QueryData where
  parseQuery = pure


-- | Encode an type to a full Query
class ToQuery a where
  toQuery :: a -> QueryData
  default toQuery :: (Generic a, GToQuery (Rep a)) => a -> QueryData
  toQuery = gToQuery . from


instance ToQuery QueryData where
  toQuery = id


instance ToQuery Query where
  toQuery = queryData


-- | Encode a datatype into a querystring parameter value. Used for both setting the browser url and saving data to a session. Reimplements 'ToHttpApiData' based on Show.
class ToParam a where
  toParam :: a -> Text
  default toParam :: (Show a) => a -> Text
  toParam = showQueryParam


-- | Decode a datatype into a querystring parameter value. Used for both setting the browser url and saving data to a session. Reimplements 'FromHttpApiData based on Show.
class FromParam a where
  parseParam :: Text -> Either Text a
  default parseParam :: (Read a) => Text -> Either Text a
  parseParam = readQueryParam


instance ToParam Int where
  toParam = toQueryParam
instance FromParam Int where
  parseParam = parseQueryParam


instance ToParam Integer where
  toParam = toQueryParam
instance FromParam Integer where
  parseParam = parseQueryParam


instance ToParam Float where
  toParam = toQueryParam
instance FromParam Float where
  parseParam = parseQueryParam


instance ToParam Double where
  toParam = toQueryParam
instance FromParam Double where
  parseParam = parseQueryParam


instance ToParam Word where
  toParam = toQueryParam
instance FromParam Word where
  parseParam = parseQueryParam


instance ToParam Word8 where
  toParam = toQueryParam
instance FromParam Word8 where
  parseParam = parseQueryParam


instance ToParam Word16 where
  toParam = toQueryParam
instance FromParam Word16 where
  parseParam = parseQueryParam


instance ToParam Word32 where
  toParam = toQueryParam
instance FromParam Word32 where
  parseParam = parseQueryParam


instance ToParam Word64 where
  toParam = toQueryParam
instance FromParam Word64 where
  parseParam = parseQueryParam


instance ToParam Bool where
  toParam = toQueryParam
instance FromParam Bool where
  parseParam = parseQueryParam


instance ToParam Text where
  toParam = toQueryParam
instance FromParam Text where
  parseParam = parseQueryParam


instance ToParam Char where
  toParam = toQueryParam
instance FromParam Char where
  parseParam = parseQueryParam


instance ToParam UTCTime where
  toParam = toQueryParam
instance FromParam UTCTime where
  parseParam = parseQueryParam


instance (Show a) => ToParam [a] where
  toParam = showQueryParam
instance (Read a) => FromParam [a] where
  parseParam = readQueryParam


instance (ToParam a) => ToParam (Maybe a) where
  toParam Nothing = ""
  toParam (Just a) = toParam a
instance (FromParam a) => FromParam (Maybe a) where
  parseParam "" = pure Nothing
  parseParam t = Just <$> parseParam @a t


instance (ToParam a, ToParam b) => ToParam (Either a b) where
  toParam (Left a) = toParam a
  toParam (Right b) = toParam b
instance (FromParam a, FromParam b) => FromParam (Either a b) where
  parseParam t =
    case parseParam @a t of
      Right a -> pure $ Left a
      Left _ -> do
        case parseParam @b t of
          Left _ -> Left $ "Could not parseParam Either: " <> t
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


-- | Parse a Traversable (list) of params
parseParams :: (Traversable t, FromParam a) => t Text -> Either Text (t a)
parseParams = traverse parseParam


-- | Generic decoding of records from a Query
class GFromQuery f where
  gParseQuery :: QueryData -> Either Text (f p)


instance (GFromQuery f, GFromQuery g) => GFromQuery (f :*: g) where
  gParseQuery q = do
    a <- gParseQuery q
    b <- gParseQuery q
    pure $ a :*: b


instance (GFromQuery f) => GFromQuery (M1 D d f) where
  gParseQuery q = M1 <$> gParseQuery q


instance (GFromQuery f) => GFromQuery (M1 C c f) where
  gParseQuery q = M1 <$> gParseQuery q


instance (Selector s, FromParam a, DefaultParam a) => GFromQuery (M1 S s (K1 R a)) where
  gParseQuery q = do
    let s = selName (undefined :: M1 S s (K1 R (f a)) p)
    let mval = lookup (pack s) q
    pure $ M1 $ K1 $ fromMaybe defaultParam mval


-- | Generic encoding of records to a Query
class GToQuery f where
  gToQuery :: f p -> QueryData


instance (GToQuery f, GToQuery g) => GToQuery (f :*: g) where
  gToQuery (f :*: g) = gToQuery f <> gToQuery g


instance (GToQuery f) => GToQuery (M1 D d f) where
  gToQuery (M1 f) = gToQuery f


instance (GToQuery f) => GToQuery (M1 C d f) where
  gToQuery (M1 f) = gToQuery f


instance (Selector s, ToParam a, Eq a, DefaultParam a) => GToQuery (M1 S s (K1 R a)) where
  gToQuery (M1 (K1 a))
    | a == defaultParam = mempty
    | otherwise =
        let sel = pack $ selName (undefined :: M1 S s (K1 R (f a)) p)
         in singleton sel (toParam a)


-- | Data.Default doesn't have a Text instance! This is better than an orphan instance
class DefaultParam a where
  defaultParam :: a
  default defaultParam :: (Default a) => a
  defaultParam = def


instance {-# OVERLAPPABLE #-} (Default a) => DefaultParam a where
  defaultParam = def


instance {-# OVERLAPS #-} DefaultParam Text where
  defaultParam = ""
