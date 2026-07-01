module Web.Hyperbole.Effect.Query where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Web.Hyperbole.Data.QueryData (FromQuery (..), Param (..), QueryData (..), ToQuery (..), queryData)
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..), request)
import Web.Hyperbole.Types.Client (Client (..), clientSetQuery)
import Web.Hyperbole.Types.Request hiding (Param)
import Web.Hyperbole.Types.Response
import Prelude


{- ! Parse querystring from the 'Request' into a datatype. See 'FromQuery'

@
#EMBED Example.Docs.Params data Filters

#EMBED Example.Docs.Params page
@
-}

{- | Parse querystring from the 'Request' into a datatype. See 'FromQuery'

@
data Filters = Filters
  { search :: Text
  }
  deriving ('ToQuery', 'FromQuery', Generic)

page :: ('Hyperbole' :> es) => 'Page' es '[Todos]
page = do
  filters <- query @Filters
  todos <- loadTodos filters
  pure $ do
    'hyper' Todos $ todosView todos
@
-}
query :: (FromQuery a, Hyperbole :> es) => Eff es a
query = do
  q <- queryParams
  case parseQuery q of
    Left e -> send $ RespondNow $ Err $ ErrQuery $ "Query Parse " <> e <> " from " <> cs (show q)
    Right a -> pure a


{- ! Update the client's querystring to an encoded datatype. See 'ToQuery'

@
#EMBED Example.Docs.Params instance HyperView Todos
@
-}

{- | Update the client's querystring to an encoded datatype. See 'ToQuery'

@
instance 'HyperView' Todos es where
  data 'Action' Todos
    = SetSearch Text
    deriving (Generic, 'ViewAction')

  'update' (SetSearch term) = do
    let filters = Filters term
    setQuery filters
    todos <- loadTodos filters
    pure $ todosView todos
@
-}
setQuery :: (ToQuery a, Hyperbole :> es) => a -> Eff es ()
setQuery a = do
  modifyQueryData (const $ toQuery a)


modifyQuery :: (ToQuery a, FromQuery a, Default a, Hyperbole :> es) => (a -> a) -> Eff es a
modifyQuery f = do
  s <- query
  let updated = f s
  setQuery updated
  pure updated


clearQuery :: (Hyperbole :> es) => Eff es ()
clearQuery =
  setQuery (mempty :: QueryData)


{- ! Parse a single query parameter. Return a 400 status if missing or if parsing fails. See 'decodeParam'

@
#EMBED Example.Docs.Params page'
@
-}

{- | Parse a single query parameter. Return a 400 status if missing or if parsing fails. See 'decodeParam'

@
page' :: ('Hyperbole' :> es) => 'Page' es '[Message]
page' = do
  msg <- param \"message\"
  pure $ do
    'hyper' Message $ messageView msg
@
-}
param :: (FromJSON a, Hyperbole :> es) => Param -> Eff es a
param p = do
  q <- queryParams
  case QueryData.require p q of
    Left e -> send $ RespondNow $ Err $ ErrQuery (cs e)
    Right a -> pure a


{- ! Parse a single parameter from the query string if available

@
#EMBED Example.Docs.SideEffects page
@
-}

{- | Parse a single parameter from the query string if available

@
page :: ('Hyperbole' :> es, Concurrent :> es, Reader Text :> es) => 'Page' es '[SlowReader]
page = do
  pure $ 'hyper' SlowReader $ messageView \"...\"
@
-}
lookupParam :: (FromJSON a, Hyperbole :> es) => Param -> Eff es (Maybe a)
lookupParam p = do
  QueryData.lookup p <$> queryParams


{- ! Modify the client's querystring to set a single parameter. See 'encodeParam'

@
#EMBED Example.Docs.Params instance HyperView Message
@
-}

{- | Modify the client's querystring to set a single parameter. See 'encodeParam'

@
instance 'HyperView' Message es where
  data 'Action' Message
    = SetMessage Text
    deriving (Generic, 'ViewAction')

  'update' (SetMessage msg) = do
    'setParam' \"message\" msg
    pure $ messageView msg
@
-}
setParam :: (ToJSON a, Hyperbole :> es) => Param -> a -> Eff es ()
setParam key a = do
  modifyQueryData (QueryData.insert key a)


-- | Delete a single parameter from the query string
deleteParam :: (Hyperbole :> es) => Param -> Eff es ()
deleteParam key = do
  modifyQueryData (QueryData.delete key)


-- | Return the querystring from 'Request' as a 'QueryData'
queryParams :: (Hyperbole :> es) => Eff es QueryData
queryParams = do
  cq <- clientQuery
  rq <- requestQuery
  pure $ fromMaybe rq cq
 where
  clientQuery = (.query) <$> send GetClient

  requestQuery :: (Hyperbole :> es) => Eff es QueryData
  requestQuery = do
    r <- request
    pure $ queryData $ filter (not . isSystemParam) r.query

  isSystemParam (key, _) =
    "hyp-" `BS.isPrefixOf` key


modifyQueryData :: (Hyperbole :> es) => (QueryData -> QueryData) -> Eff es ()
modifyQueryData f = do
  q <- queryParams
  send $ ModClient $ clientSetQuery (f q)
