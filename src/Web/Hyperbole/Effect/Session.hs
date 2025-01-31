{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.Effect.Session where

import Data.Maybe (fromMaybe)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Data.QueryData
import Web.Hyperbole.Data.Session as Cookies
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Request (request)
import Web.Hyperbole.Effect.Server (Client (..), Request (..), Response (..), ResponseError (..))
import Prelude


{- | Persist datatypes in browser cookies. If the session doesn't exist, the 'DefaultParam' is used

@
#EMBED Example/Docs/Sessions.hs data Preferences

#EMBED Example/Docs/Sessions.hs instance DefaultParam Preferences

#EMBED Example/Docs/Sessions.hs page
@
-}
session :: (Session a, DefaultParam a, FromParam a, Hyperbole :> es) => Eff es a
session = do
  ms <- lookupSession
  pure $ fromMaybe defaultParam ms


-- | Return a session if it exists
lookupSession :: forall a es. (Session a, FromParam a, Hyperbole :> es) => Eff es (Maybe a)
lookupSession = do
  let key = sessionKey @a
  mck <- Cookies.lookup key <$> sessionCookies
  case mck of
    Nothing -> pure Nothing
    Just val -> parseSession key val


{- | Persist datatypes in browser cookies

@
#EMBED Example/Docs/Sessions.hs data Preferences

#EMBED Example/Docs/Sessions.hs instance DefaultParam Preferences

#EMBED Example/Docs/Sessions.hs instance HyperView Content
@
-}
saveSession :: (Session a, ToParam a, Hyperbole :> es) => a -> Eff es ()
saveSession a = do
  modifyCookies $ Cookies.insert (sessionCookie a)


modifySession :: (Session a, DefaultParam a, ToParam a, FromParam a, Hyperbole :> es) => (a -> a) -> Eff es a
modifySession f = do
  s <- session
  let updated = f s
  saveSession updated
  pure updated


modifySession_ :: (Session a, DefaultParam a, ToParam a, FromParam a, Hyperbole :> es) => (a -> a) -> Eff es ()
modifySession_ f = do
  _ <- modifySession f
  pure ()


-- | Remove a single 'Session' from the browser cookies
deleteSession :: forall a es. (Session a, Hyperbole :> es) => Eff es ()
deleteSession = do
  modifyCookies $ Cookies.insert (deletedCookie @a)


parseSession :: (FromParam a, Hyperbole :> es) => Param -> ParamValue -> Eff es a
parseSession prm val = do
  case parseParam val of
    Left e -> send $ RespondEarly $ Err $ ErrSession prm e
    Right a -> pure a


-- | save a single datatype to a specific key in the session
setCookie :: (ToParam a, Hyperbole :> es) => Cookie -> Eff es ()
setCookie ck = do
  modifyCookies (Cookies.insert ck)


-- | Modify the client cookies
modifyCookies :: (Hyperbole :> es) => (Cookies -> Cookies) -> Eff es ()
modifyCookies f =
  send $ ModClient $ \client ->
    Client{session = f client.session, query = client.query, requestId = client.requestId}


-- | Return all the cookies, both those sent in the request and others added by the page
sessionCookies :: (Hyperbole :> es) => Eff es Cookies
sessionCookies = do
  clt <- clientSessionCookies
  req <- requestSessionCookies
  pure $ clt <> req


-- | Return the session from the Client cookies
clientSessionCookies :: (Hyperbole :> es) => Eff es Cookies
clientSessionCookies = do
  (.session) <$> send GetClient


-- | Return the session from the 'Request' cookies
requestSessionCookies :: (Hyperbole :> es) => Eff es Cookies
requestSessionCookies = do
  (.cookies) <$> request
