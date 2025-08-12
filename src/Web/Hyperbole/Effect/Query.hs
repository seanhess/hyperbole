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
import Web.Hyperbole.Effect.Page


{- | Parse querystring from the 'Request' into a datatype. See 'FromQuery'

@
#EMBED Example/Docs/Params.hs data Filters

#EMBED Example/Docs/Params.hs page
@
-}
query :: (FromQuery a, Page :> es) => Eff es a
query = do
  q <- queryParams
  case parseQuery q of
    Left e -> pageError $ BadQuery (cs e) q
    Right a -> pure a


{- | Update the client's querystring to an encoded datatype. See 'ToQuery'

@
#EMBED Example/Docs/Params.hs instance HyperView Todos
@
-}
setQuery :: (ToQuery a, Page :> es) => a -> Eff es ()
setQuery a = do
  modQueryData (const $ toQuery a)


modQuery :: (ToQuery a, FromQuery a, Default a, Page :> es) => (a -> a) -> Eff es a
modQuery f = do
  s <- query
  let updated = f s
  setQuery updated
  pure updated


{- | Parse a single query parameter. Return a 400 status if missing or if parsing fails. See 'FromParam'

@
#EMBED Example/Docs/Params.hs page'
@
-}
param :: (FromParam a, Page :> es) => Param -> Eff es a
param p = do
  q <- queryParams
  case QueryData.require p q of
    Left e -> pageError $ BadQueryParam (cs e) p q
    Right a -> pure a


-- | Parse a single parameter from the query string if available
lookupParam :: (FromParam a, Page :> es) => Param -> Eff es (Maybe a)
lookupParam p = do
  QueryData.lookup p <$> queryParams


{- | Modify the client's querystring to set a single parameter. See 'ToParam'

@
#EMBED Example/Docs/Params.hs instance HyperView Message
@
-}
setParam :: (ToParam a, Page :> es) => Param -> a -> Eff es ()
setParam key a = do
  modQueryData (QueryData.insert key a)


-- | Delete a single parameter from the query string
deleteParam :: (Page :> es) => Param -> Eff es ()
deleteParam key = do
  modQueryData (QueryData.delete key)


-- | Return the query from 'Request' as a 'QueryData'
queryParams :: (Page :> es) => Eff es QueryData
queryParams = do
  (.query) <$> client


-- -- eh.....
-- requestQuery :: (Page :> es) => Eff es QueryData
-- requestQuery = do
--   r <- request
--   pure $ queryData $ filter (not . isSystemParam) r.query

-- isSystemParam (key, _) =
--   "hyp-" `BS.isPrefixOf` key

modQueryData :: (Page :> es) => (QueryData -> QueryData) -> Eff es ()
modQueryData f = do
  q <- queryParams
  modClient $ \Client{session} ->
    Client{query = f q, session}
