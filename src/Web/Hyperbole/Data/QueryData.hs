{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Data.QueryData where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (..), fromJSON)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Text qualified as T
import GHC.Exts (IsList (..))
import GHC.Generics
import Network.HTTP.Types (Query, QueryItem, renderQuery)
import Network.HTTP.Types qualified as HTTP
import Prelude hiding (lookup)


newtype Param = Param {text :: Text}
  deriving newtype (Show, Eq, Ord, IsString)


-- | Key-value store for query params and sessions
newtype QueryData = QueryData (Map Param Text)
  deriving (Show)
  deriving newtype (Monoid, Semigroup)


instance IsList QueryData where
  type Item QueryData = (Param, Text)
  fromList = QueryData . fromList
  toList (QueryData m) = toList m


parseParam :: (FromJSON a) => Value -> Either String a
parseParam v =
  case fromJSON v of
    Success a -> pure a
    Error e -> Left e


-- the "default" encoding uses unquoted strings
encodeParam :: (ToJSON a) => a -> Text
encodeParam a = do
  case A.toJSON a of
    String s -> cs s
    val -> cs $ A.encode val


decodeParam :: (FromJSON a) => Text -> Either String a
decodeParam inp = maybe (Left $ "Could not decode Query Param: " <> cs inp) pure $ do
  A.decode (cs inp) <|> A.decode (cs $ "\"" <> inp <> "\"")


singleton :: (ToJSON a) => Param -> a -> QueryData
singleton key a = QueryData $ M.singleton key (encodeParam a)


insert :: (ToJSON a) => Param -> a -> QueryData -> QueryData
insert p a (QueryData m) =
  QueryData $ M.insert p (encodeParam a) m


insertAll :: (ToQuery a) => a -> QueryData -> QueryData
insertAll a (QueryData m) =
  let QueryData kvs = toQuery a
   in QueryData $ M.union kvs m


delete :: Param -> QueryData -> QueryData
delete p (QueryData m) =
  QueryData $ M.delete p m


lookup :: (FromJSON a) => Param -> QueryData -> Maybe a
lookup k (QueryData m) = do
  t :: Text <- M.lookup k m
  either (const Nothing) pure $ decodeParam t


require :: (FromJSON a) => Param -> QueryData -> Either String a
require p (QueryData m) = do
  case M.lookup p m of
    Nothing -> Left $ "Missing Key: " <> cs p.text
    Just val -> decodeParam val


filterKey :: (Param -> Bool) -> QueryData -> QueryData
filterKey p (QueryData m) =
  QueryData $ M.filterWithKey (\k _ -> p k) m


member :: Param -> QueryData -> Bool
member k (QueryData qd) = M.member k qd


elems :: QueryData -> [Text]
elems (QueryData m) = M.elems m


render :: QueryData -> Text
render qd =
  T.replace "%20" "+" $ cs $ renderQuery False (HTTP.toQuery $ fromQueryData qd)


parse :: Text -> QueryData
parse =
  -- it already knows how to handle + signs on parse
  queryData . HTTP.parseQuery . cs


queryData :: Query -> QueryData
queryData q =
  fromList $ fmap fromQueryItem q
 where
  fromQueryItem :: QueryItem -> (Param, Text)
  fromQueryItem (key, mval) = do
    (Param (cs key), fromParam mval)

  -- do we include fields that don't have a value? Sure
  fromParam :: Maybe ByteString -> Text
  fromParam Nothing = ""
  fromParam (Just t) = cs t


-- Now we have our own thing!
-- 1. ALWAYS encode strings without quotes
-- 2. How do you distinguish between numbers and strings then?

-- problem: strings are always quoted
-- if we unquote them, they might be a string like "30"
fromQueryData :: QueryData -> Query
fromQueryData q =
  fmap toQueryItem $ toList q
 where
  toQueryItem :: (Param, Text) -> QueryItem
  toQueryItem (Param prm, pval) =
    (cs prm, Just $ cs pval)


{- | Decode a type from a 'QueryData'. Missing fields are set to 'Data.Default.def'

@
#EMBED Example.Docs.Encoding data Filters
@

>>> parseQuery $ QueryData.parse "active=true&search=asdf"
Right (Filters True "asdf")

>>> parseQuery $ QueryData.parse "search=asdf"
Right (Filters False "asdf")
-}
class FromQuery a where
  parseQuery :: QueryData -> Either String a
  default parseQuery :: (Generic a, GFromQuery (Rep a)) => QueryData -> Either String a
  parseQuery q = to <$> gParseQuery q


instance FromQuery QueryData where
  parseQuery = pure


{- | A page can store state in the browser 'query' string. ToQuery and 'FromQuery' control how a datatype is encoded to a full query string

@
#EMBED Example.Docs.Encoding data Filters
@

>>> QueryData.render $ toQuery $ Filter True "asdf"
"active=true&search=asdf"

If the value of a field is the same as 'Default', it will be omitted from the query string

>>> QueryData.render $ toQuery $ Filter True ""
"active=true"

>>> QueryData.render $ toQuery $ Filter False ""
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


-- | Generic decoding of records from a Query
class GFromQuery f where
  gParseQuery :: QueryData -> Either String (f p)


instance (GFromQuery f, GFromQuery g) => GFromQuery (f :*: g) where
  gParseQuery q = do
    a <- gParseQuery q
    b <- gParseQuery q
    pure $ a :*: b


instance (GFromQuery f) => GFromQuery (M1 D d f) where
  gParseQuery q = M1 <$> gParseQuery q


instance (GFromQuery f) => GFromQuery (M1 C c f) where
  gParseQuery q = M1 <$> gParseQuery q


instance {-# OVERLAPPABLE #-} (Selector s, FromJSON a, Default a) => GFromQuery (M1 S s (K1 R a)) where
  gParseQuery q = do
    let s = selName (undefined :: M1 S s (K1 R (f a)) p)
    let mval = lookup (Param $ pack s) q
    pure $ M1 $ K1 $ fromMaybe def mval


-- Text doesn't have a default instance. Annoying
instance {-# OVERLAPS #-} (Selector s) => GFromQuery (M1 S s (K1 R Text)) where
  gParseQuery q = do
    let s = selName (undefined :: M1 S s (K1 R (f a)) p)
    let mval = lookup (Param $ pack s) q
    pure $ M1 $ K1 $ fromMaybe "" mval


-- | Generic encoding of records to a Query
class GToQuery f where
  gToQuery :: f p -> QueryData


instance (GToQuery f, GToQuery g) => GToQuery (f :*: g) where
  gToQuery (f :*: g) = gToQuery f <> gToQuery g


instance (GToQuery f) => GToQuery (M1 D d f) where
  gToQuery (M1 f) = gToQuery f


instance (GToQuery f) => GToQuery (M1 C d f) where
  gToQuery (M1 f) = gToQuery f


instance {-# OVERLAPPABLE #-} (Selector s, ToJSON a, Eq a, Default a) => GToQuery (M1 S s (K1 R a)) where
  gToQuery (M1 (K1 a))
    | a == def = mempty
    | otherwise =
        let sel = Param $ pack $ selName (undefined :: M1 S s (K1 R (f a)) p)
         in singleton sel a


-- Special case for Text, which has no Default instance
instance {-# OVERLAPS #-} (Selector s) => GToQuery (M1 S s (K1 R Text)) where
  gToQuery (M1 (K1 a))
    | a == "" = mempty
    | otherwise =
        let sel = Param $ pack $ selName (undefined :: M1 S s (K1 R (f a)) p)
         in singleton sel a

-- instance {-# OVERLAPS #-} (Selector s, ToJSON a, Eq a) => GToQuery (M1 S s (K1 R [a])) where
--   gToQuery (M1 (K1 a))
--     | a == [] = mempty
--     | otherwise =
--         let sel = Param $ pack $ selName (undefined :: M1 S s (K1 R (f a)) p)
--          in singleton sel $ Plusses a
