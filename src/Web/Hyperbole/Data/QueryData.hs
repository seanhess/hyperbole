{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Data.QueryData where

import Data.Aeson (Value (Null))
import Data.ByteString (ByteString)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import GHC.Exts (IsList (..))
import GHC.Generics
import Network.HTTP.Types (Query, QueryItem, renderQuery)
import Network.HTTP.Types qualified as HTTP
import Web.Hyperbole.Data.Encoded (decodeParam, encodeParam)
import Web.Hyperbole.Data.Param
import Prelude hiding (lookup)


-- | Key-value store for query params and sessions
newtype QueryData = QueryData (Map Param ParamValue)
  deriving (Show)
  deriving newtype (Monoid, Semigroup)


instance IsList QueryData where
  type Item QueryData = (Param, ParamValue)
  fromList = QueryData . fromList
  toList (QueryData m) = toList m


singleton :: (ToParam a) => Param -> a -> QueryData
singleton key a = QueryData $ M.singleton key (toParam a)


insert :: (ToParam a) => Param -> a -> QueryData -> QueryData
insert p a (QueryData m) =
  QueryData $ M.insert p (toParam a) m


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


require :: (FromParam a) => Param -> QueryData -> Either String a
require p (QueryData m) = do
  case M.lookup p m of
    Nothing -> Left $ "Missing Key: " <> cs p.text
    Just val -> parseParam val


filterKey :: (Param -> Bool) -> QueryData -> QueryData
filterKey p (QueryData m) =
  QueryData $ M.filterWithKey (\k _ -> p k) m


member :: Param -> QueryData -> Bool
member k (QueryData qd) = M.member k qd


elems :: QueryData -> [ParamValue]
elems (QueryData m) = M.elems m


render :: QueryData -> ByteString
render qd =
  renderQuery False (HTTP.toQuery $ fromQueryData qd)


parse :: ByteString -> QueryData
parse =
  -- urlDecode True
  queryData . HTTP.parseQuery


queryData :: Query -> QueryData
queryData q =
  fromList $ fmap fromQueryItem q
 where
  fromQueryItem :: QueryItem -> (Param, ParamValue)
  fromQueryItem (key, mval) =
    (Param (cs key), fromParam mval)

  fromParam :: Maybe ByteString -> ParamValue
  fromParam Nothing = jsonParam Null
  fromParam (Just t) = decodeParam (cs t)


fromQueryData :: QueryData -> Query
fromQueryData q =
  fmap toQueryItem $ toList q
 where
  toQueryItem :: (Param, ParamValue) -> QueryItem
  toQueryItem (Param prm, pval) =
    (cs prm, Just $ toQueryValue pval)

  toQueryValue :: ParamValue -> ByteString
  toQueryValue = cs . encodeParam


{- | Decode a type from a 'QueryData'. Missing fields are set to 'Data.Default.def'

@
#EMBED Example/Docs/Encoding.hs data Filters
@

>>> parseQuery $ parse "active=true&search=asdf"
Right (Filters True "asdf")

>>> parseQuery $ parse "search=asdf"
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


instance {-# OVERLAPPABLE #-} (Selector s, FromParam a, Default a) => GFromQuery (M1 S s (K1 R a)) where
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


instance {-# OVERLAPPABLE #-} (Selector s, ToParam a, Eq a, Default a) => GToQuery (M1 S s (K1 R a)) where
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

-- instance {-# OVERLAPS #-} (Selector s, ToParam a, Eq a) => GToQuery (M1 S s (K1 R [a])) where
--   gToQuery (M1 (K1 a))
--     | a == [] = mempty
--     | otherwise =
--         let sel = Param $ pack $ selName (undefined :: M1 S s (K1 R (f a)) p)
--          in singleton sel $ Plusses a
