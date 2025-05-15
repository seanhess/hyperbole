module Web.Hyperbole.Effect.Query where

import Data.ByteString qualified as BS
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Web.Hyperbole.Data.Param (FromParam (..), Param, ToParam (..))
import Web.Hyperbole.Data.QueryData (FromQuery (..), QueryData (..), ToQuery (..), queryData)
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Request (request)
import Web.Hyperbole.Effect.Server (Client (..), Request (..), Response (..), ResponseError (..))
import Prelude


{- | Parse querystring from the 'Request' into a datatype. See 'FromQuery'

@
#EMBED Example/Docs/Params.hs data Filters

#EMBED Example/Docs/Params.hs page
@
-}
query :: (FromQuery a, Hyperbole :> es) => Eff es a
query = do
  q <- queryParams
  case parseQuery q of
    Left e -> send $ RespondEarly $ Err $ ErrQuery $ "Query Parse " <> e <> " from " <> cs (show q)
    Right a -> pure a


{- | Update the client's querystring to an encoded datatype. See 'ToQuery'

@
#EMBED Example/Docs/Params.hs instance HyperView Todos
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


{- | Parse a single query parameter. Return a 400 status if missing or if parsing fails. See 'FromParam'

@
#EMBED Example/Docs/Params.hs page'
@
-}
param :: (FromParam a, Hyperbole :> es) => Param -> Eff es a
param p = do
  q <- queryParams
  case QueryData.require p q of
    Left e -> send $ RespondEarly $ Err $ ErrQuery e
    Right a -> pure a


-- | Parse a single parameter from the query string if available
lookupParam :: (FromParam a, Hyperbole :> es) => Param -> Eff es (Maybe a)
lookupParam p = do
  QueryData.lookup p <$> queryParams


{- | Modify the client's querystring to set a single parameter. See 'ToParam'

@
#EMBED Example/Docs/Params.hs instance HyperView Message
@
-}
setParam :: (ToParam a, Hyperbole :> es) => Param -> a -> Eff es ()
setParam key a = do
  modifyQueryData (QueryData.insert key a)


-- | Delete a single parameter from the query string
deleteParam :: (Hyperbole :> es) => Param -> Eff es ()
deleteParam key = do
  modifyQueryData (QueryData.delete key)


-- | Return the query from 'Request' as a 'QueryData'
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
  send $ ModClient $ \Client{session, requestId} ->
    Client{query = Just $ f q, session, requestId}
