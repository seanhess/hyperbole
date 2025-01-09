module Web.Hyperbole.Effect.Query where

import Data.String.Conversions (cs)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Web.Hyperbole.Data.QueryData (FromParam (..), FromQuery (..), Param, QueryData (..), ToParam (..), ToQuery (..))
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Client (..), Response (..), ResponseError (..))
import Prelude


{- | Parse querystring from the 'Request' into a datatype. See 'FromQuery'

@
#EMBED Example/Docs/Encoding.hs data Filters

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
  modifyQuery (const $ toQuery a)


{- | Parse a single query parameter. Return a 400 status if missing or if parsing fails. See 'FromParam'

@
#EMBED Example/Docs/Params.hs page
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
  modifyQuery (QueryData.insert key a)


-- | Delete a single parameter from the query string
deleteParam :: (Hyperbole :> es) => Param -> Eff es ()
deleteParam key = do
  modifyQuery (QueryData.delete key)


-- | Return the query from 'Request' as a 'QueryData'
queryParams :: (Hyperbole :> es) => Eff es QueryData
queryParams = do
  (.query) <$> send GetClient


modifyQuery :: (Hyperbole :> es) => (QueryData -> QueryData) -> Eff es ()
modifyQuery f =
  send $ ModClient $ \client ->
    Client{query = f client.query, session = client.session}
