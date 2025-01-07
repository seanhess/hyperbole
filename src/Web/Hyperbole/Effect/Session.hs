module Web.Hyperbole.Effect.Session where

import Data.String.Conversions (cs)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Web.Hyperbole.Data.QueryData (FromParam (..), FromQuery (..), Param (..), QueryData (..), ToParam (..), ToQuery (..))
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Client (..), Response (..), ResponseError (..), Session)
import Prelude


{- | Parse data from the client's session into a datatype. See 'FromQuery'

@
#EMBED Example/Docs/Sessions.hs data Preferences

#EMBED Example/Docs/Sessions.hs page
@
-}
session :: (FromQuery a, Hyperbole :> es) => Eff es a
session = do
  q <- sessionParams
  case parseQuery q of
    Left e -> send $ RespondEarly $ Err $ ErrSession $ "Query Parse " <> e <> " from " <> cs (show q)
    Right a -> pure a


{- | Update the client's session to a datatype encoded with 'ToQuery'

@
#EMBED Example/Docs/Sessions.hs instance HyperView Content
@
-}
setSession :: (Hyperbole :> es, ToQuery a) => a -> Eff es ()
setSession a = do
  modifySession (QueryData.insertAll a)


-- | parse a single datatype from the session. Return a 400 status if missing or parsing fails. See 'FromParam'
sessionKey :: (FromParam a, Hyperbole :> es) => Param -> Eff es a
sessionKey k = do
  s <- sessionParams
  case QueryData.require k s of
    Left e -> send $ RespondEarly $ Err $ ErrSession e
    Right a -> pure a


-- | Parse a single parameter from the session if available
lookupSessionKey :: (FromParam a, Hyperbole :> es) => Param -> Eff es (Maybe a)
lookupSessionKey k = do
  QueryData.lookup k <$> sessionParams


-- | save a single datatype to a specific key in the session
setSessionKey :: (ToParam a, Hyperbole :> es) => Param -> a -> Eff es ()
setSessionKey k v = do
  modifySession (QueryData.insert k v)


-- | Delete a single key from the session
deleteSessionKey :: (Hyperbole :> es) => Param -> Eff es ()
deleteSessionKey k = do
  modifySession (QueryData.delete k)


modifySession :: (Hyperbole :> es) => (Session -> Session) -> Eff es ()
modifySession f =
  send $ ModClient $ \client ->
    Client{session = f client.session, query = client.query}


-- | Return the session from the 'Request' cookies as a 'QueryData'
sessionParams :: (Hyperbole :> es) => Eff es QueryData
sessionParams = do
  (.session) <$> send GetClient
