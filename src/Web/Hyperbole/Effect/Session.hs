{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Effect.Session where

import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics
import Web.Hyperbole.Data.QueryData
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Server (Client (..))
import Prelude


{- | Parse data from the client's session into a datatype. See 'FromQuery'

@
#EMBED Example/Docs/Sessions.hs data Preferences

#EMBED Example/Docs/Sessions.hs page
@
-}
session :: (Session a, Default a, FromParam a, Hyperbole :> es) => Eff es a
session = do
  ms <- lookupSession
  pure $ fromMaybe def ms


lookupSession :: forall a es. (Session a, FromParam a, Hyperbole :> es) => Eff es (Maybe a)
lookupSession = do
  s <- sessionParams
  let key = sessionKey @a
  pure $ QueryData.lookup key s


{- | Update the client's session to a datatype encoded with 'ToQuery'

@
#EMBED Example/Docs/Sessions.hs instance HyperView Content
@
-}
saveSession :: forall a es. (Session a, ToParam a, Hyperbole :> es) => a -> Eff es ()
saveSession a = do
  let key = sessionKey @a
  setSessionParam key a


modifySession :: (Session a, Default a, ToParam a, FromParam a, Hyperbole :> es) => (a -> a) -> Eff es a
modifySession f = do
  s <- session
  let updated = f s
  saveSession updated
  pure updated


modifySession_ :: (Session a, Default a, ToParam a, FromParam a, Hyperbole :> es) => (a -> a) -> Eff es ()
modifySession_ f = do
  _ <- modifySession f
  pure ()


{- | parse a single datatype from the session. Return a 400 status if missing or parsing fails. See 'FromParam'
sessionKey :: (FromParam a, Hyperbole :> es) => Param -> Eff es a
sessionKey k = do
  s <- sessionParams
  case QueryData.require k s of
    Left e -> send $ RespondEarly $ Err $ ErrSession e
    Right a -> pure a
-}

-- | Parse a single parameter from the session if available
lookupSessionParam :: (FromParam a, Hyperbole :> es) => Param -> Eff es (Maybe a)
lookupSessionParam k = do
  QueryData.lookup k <$> sessionParams


-- | save a single datatype to a specific key in the session
setSessionParam :: (ToParam a, Hyperbole :> es) => Param -> a -> Eff es ()
setSessionParam k v = do
  modifySessionParams (QueryData.insert k v)


-- | Delete a single key from the session
deleteSessionParam :: (Hyperbole :> es) => Param -> Eff es ()
deleteSessionParam k = do
  modifySessionParams (QueryData.delete k)


modifySessionParams :: (Hyperbole :> es) => (QueryData -> QueryData) -> Eff es ()
modifySessionParams f =
  send $ ModClient $ \client ->
    Client{session = f client.session, query = client.query}


-- | Return the session from the 'Request' cookies as a 'QueryData'
sessionParams :: (Hyperbole :> es) => Eff es QueryData
sessionParams = do
  (.session) <$> send GetClient


-- you have to define a default?
class Session a where
  sessionKey :: Param
  default sessionKey :: (Generic a, GDatatypeName (Rep a)) => Param
  sessionKey = Param $ gDatatypeName $ from (undefined :: a)


-- | generic datatype name
class GDatatypeName f where
  gDatatypeName :: f p -> Text


instance (Datatype d) => GDatatypeName (M1 D d f) where
  gDatatypeName _ =
    cs $ datatypeName (undefined :: M1 D d f p)


{- | Discarded Session Side Effect, analagous to State
data Session s :: Effect where
  GetSession :: Session s m s
  PutSession :: s -> Session s m ()
  ModSession :: (s -> s) -> Session s m s


type instance DispatchOf (Session s) = 'Dynamic


-- | Run a session with a default value
runSession
  :: (Hyperbole :> es, SessionKey s, FromParam s, ToParam s)
  => s
  -> Eff (Session s : es) a
  -> Eff es a
runSession def = interpret $ \_ -> \case
  GetSession -> do
    ms <- lookupSession
    pure $ fromMaybe def ms
  PutSession s ->
    saveSession s
  ModSession f -> do
    s <- fromMaybe def <$> lookupSession
    let updated = f s
    saveSession updated
    pure updated


getSession :: (Session s :> es) => Eff es s
getSession = send GetSession


putSession :: (Session s :> es) => s -> Eff es ()
putSession s = send $ PutSession s


modifySession :: (Session s :> es) => (s -> s) -> Eff es s
modifySession f = send $ ModSession f


modifySession_ :: (Session s :> es) => (s -> s) -> Eff es ()
modifySession_ f = do
  _ <- modifySession f
  pure ()
-}
