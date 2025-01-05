module Web.Hyperbole.Effect.Session where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Web.Hyperbole.Data.QueryData (FromParam (..), FromQuery (..), QueryData (..), ToParam (..), ToQuery (..))
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Client (..), Response (..), ResponseError (..), Session)
import Prelude


session :: (FromQuery a, Hyperbole :> es) => Eff es a
session = do
  q <- sessionParams
  case parseQuery q of
    Left e -> send $ RespondEarly $ Err $ ErrSession $ "Query Parse " <> e <> " from " <> cs (show q)
    Right a -> pure a


setSession :: (Hyperbole :> es, ToQuery a) => a -> Eff es ()
setSession a = do
  modifySession (QueryData.insertAll a)


-- | Lookup a session variable by keyword
sessionKey :: (FromParam a, Hyperbole :> es) => Text -> Eff es a
sessionKey k = do
  s <- sessionParams
  case QueryData.require k s of
    Left e -> send $ RespondEarly $ Err $ ErrSession e
    Right a -> pure a


lookupSessionKey :: (FromParam a, Hyperbole :> es) => Text -> Eff es (Maybe a)
lookupSessionKey k = do
  QueryData.lookup k <$> sessionParams


-- | Set a session variable by keyword
setSessionKey :: (ToParam a, Hyperbole :> es) => Text -> a -> Eff es ()
setSessionKey k v = do
  modifySession (QueryData.insert k v)


-- | Clear a session variable
deleteSessionKey :: (Hyperbole :> es) => Text -> Eff es ()
deleteSessionKey k = do
  modifySession (QueryData.delete k)


modifySession :: (Hyperbole :> es) => (Session -> Session) -> Eff es ()
modifySession f =
  send $ ModClient $ \client ->
    Client{session = f client.session, query = client.query}


sessionParams :: (Hyperbole :> es) => Eff es QueryData
sessionParams = do
  (.session) <$> send GetClient
