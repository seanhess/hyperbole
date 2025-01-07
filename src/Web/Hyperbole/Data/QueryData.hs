{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Data.QueryData where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Word
import GHC.Generics
import Network.HTTP.Types (Query, renderQuery)
import Network.HTTP.Types qualified as HTTP
import Text.Read (readMaybe)
import Web.HttpApiData (parseQueryParam, toQueryParam)
import Prelude hiding (lookup)


newtype Param = Param {text :: Text}
  deriving newtype (Show, Eq, Ord, IsString)


newtype ParamValue = ParamValue {text :: Text}
  deriving newtype (Show, Eq, Ord, IsString)


-- | Key-value store for query params and sessions
newtype QueryData = QueryData (Map Param ParamValue)
  deriving (Show)
  deriving newtype (Monoid, Semigroup)


singleton :: (ToParam a) => Param -> a -> QueryData
singleton key a = QueryData $ M.singleton key (toParam a)


insert :: (ToParam a) => Param -> a -> QueryData -> QueryData
insert p a (QueryData m) =
  let val = toParam a
   in QueryData $ M.insert p val m


insertAll :: (ToQuery a) => a -> QueryData -> QueryData
insertAll a (QueryData m) =
  let QueryData kvs = toQuery a
   in QueryData $ M.union kvs m


delete :: Param -> QueryData -> QueryData
delete p (QueryData m) =
  QueryData $ M.delete p m


lookup :: (FromParam a) => Param -> QueryData -> Maybe a
lookup k (QueryData m) = do
  t <- M.lookup k m
  either (const Nothing) pure $ parseParam t


require :: (FromParam a) => Param -> QueryData -> Either Text a
require p qd = do
  case lookup p qd of
    Nothing -> Left $ "Missing Key: " <> p.text
    Just val -> val


filterKey :: (Param -> Bool) -> QueryData -> QueryData
filterKey p (QueryData m) =
  QueryData $ M.filterWithKey (\k _ -> p k) m


member :: Param -> QueryData -> Bool
member k (QueryData qd) = M.member k qd


elems :: QueryData -> [ParamValue]
elems (QueryData m) = M.elems m


render :: QueryData -> ByteString
render (QueryData m) =
  -- urlEncode True
  renderQuery False (HTTP.toQuery $ fmap queryItem $ M.toList m)
 where
  queryItem (Param k, ParamValue val) = (k, val)


parse :: ByteString -> QueryData
parse =
  -- urlDecode True
  queryData . HTTP.parseQuery


queryData :: Query -> QueryData
queryData =
  QueryData . M.fromList . map (bimap (Param . cs) value)
 where
  -- empty / missing values are encoded as empty strings
  value Nothing = ""
  value (Just v) = ParamValue (cs v)


{- | Decode a type from a 'QueryData'. Missing fields are set to 'defaultParam'

@
#EMBED Example/Docs/Encoding.hs data Filters
@

>>> parseQuery $ parse "active=true&search=asdf"
Right (Filters True "asdf")

>>> parseQuery $ parse "search=asdf"
Right (Filters False "asdf")
-}
class FromQuery a where
  parseQuery :: QueryData -> Either Text a
  default parseQuery :: (Generic a, GFromQuery (Rep a)) => QueryData -> Either Text a
  parseQuery q = to <$> gParseQuery q


instance FromQuery QueryData where
  parseQuery = pure


{- | Sessions, Query Params, and Forms all encode their data as query strings. ToQuery and 'FromQuery' control how a datatype is encoded to a full query string

@
#EMBED Example/Docs/Encoding.hs data Filters
@

>>> render $ toQuery $ Filter True "asdf"
"active=true&search=asdf"

If the value of a field is the same as 'DefaultParam', it will be omitted from the query string

>>> render $ toQuery $ Filter True ""
"active=true"

>>> render $ toQuery $ Filter False ""
""
-}
class ToQuery a where
  toQuery :: a -> QueryData
  default toQuery :: (Generic a, GToQuery (Rep a)) => a -> QueryData
  toQuery = gToQuery . from


instance ToQuery QueryData where
  toQuery = id


instance ToQuery Query where
  toQuery = queryData


{- | Encode data into a query parameter value

@
#EMBED Example/Effects/Todos.hs data Todo = Todo
@

@
#EMBED Example/Docs/Encoding.hs data Tags

#EMBED Example/Docs/Encoding.hs instance ToParam Tags
@
-}
class ToParam a where
  toParam :: a -> ParamValue
  default toParam :: (Show a) => a -> ParamValue
  toParam = showQueryParam


{- | Decode data from a query parameter value

@
#EMBED Example/Effects/Todos.hs data Todo = Todo
@

@
#EMBED Example/Docs/Encoding.hs data Tags

#EMBED Example/Docs/Encoding.hs instance FromParam Tags
@
-}
class FromParam a where
  parseParam :: ParamValue -> Either Text a
  default parseParam :: (Read a) => ParamValue -> Either Text a
  parseParam = readQueryParam


instance ToParam Int where
  toParam = ParamValue . toQueryParam
instance FromParam Int where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Integer where
  toParam = ParamValue . toQueryParam
instance FromParam Integer where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Float where
  toParam = ParamValue . toQueryParam
instance FromParam Float where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Double where
  toParam = ParamValue . toQueryParam
instance FromParam Double where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Word where
  toParam = ParamValue . toQueryParam
instance FromParam Word where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Word8 where
  toParam = ParamValue . toQueryParam
instance FromParam Word8 where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Word16 where
  toParam = ParamValue . toQueryParam
instance FromParam Word16 where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Word32 where
  toParam = ParamValue . toQueryParam
instance FromParam Word32 where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Word64 where
  toParam = ParamValue . toQueryParam
instance FromParam Word64 where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Bool where
  toParam = ParamValue . toQueryParam
instance FromParam Bool where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Text where
  toParam = ParamValue . toQueryParam
instance FromParam Text where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam Char where
  toParam = ParamValue . toQueryParam
instance FromParam Char where
  parseParam (ParamValue t) = parseQueryParam t


instance ToParam UTCTime where
  toParam = ParamValue . toQueryParam
instance FromParam UTCTime where
  parseParam (ParamValue t) = parseQueryParam t


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
  parseParam val =
    case parseParam @a val of
      Right a -> pure $ Left a
      Left _ -> do
        case parseParam @b val of
          Left _ -> Left $ "Could not parseParam Either: " <> val.text
          Right b -> pure $ Right b


-- | Encode a Show as a query param
showQueryParam :: (Show a) => a -> ParamValue
showQueryParam a = ParamValue $ toQueryParam $ show a


-- | Decode a Read as a query param
readQueryParam :: (Read a) => ParamValue -> Either Text a
readQueryParam (ParamValue t) = do
  str <- parseQueryParam t
  case readMaybe str of
    Nothing -> Left $ pack $ "Could not read query param: " <> str
    Just a -> pure a


-- | Parse a Traversable (list) of params
parseParams :: (Traversable t, FromParam a) => t ParamValue -> Either Text (t a)
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
    let mval = lookup (Param $ pack s) q
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
        let sel = Param $ pack $ selName (undefined :: M1 S s (K1 R (f a)) p)
         in singleton sel a


-- | Data.Default doesn't have a Text instance! This is better than an orphan instance
class DefaultParam a where
  defaultParam :: a
  default defaultParam :: (Default a) => a
  defaultParam = def


instance {-# OVERLAPPABLE #-} (Default a) => DefaultParam a where
  defaultParam = def


instance {-# OVERLAPS #-} DefaultParam Text where
  defaultParam = ""
