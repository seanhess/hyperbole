{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Effect.Session where

import Data.Aeson as A (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Bifunctor (first)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics
import Web.Hyperbole.Data.Cookie as Cookie
import Web.Hyperbole.Data.Param
import Web.Hyperbole.Data.URI (Path)
import Web.Hyperbole.Effect.Hyperbole (Hyperbole (..))
import Web.Hyperbole.Effect.Request (request)
import Web.Hyperbole.Effect.Server (Client (..))
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response


{- | Configure a data type to persist in the 'session' as a cookie. These are type-indexed, so only one of each can exist in the session

@
#EMBED Example/Docs/Sessions.hs data Preferences

#EMBED Example/Docs/Sessions.hs instance DefaultParam Preferences
@
-}
class Session a where
  -- | Unique key for this Session Type. Defaults to the datatypeName
  sessionKey :: Key
  default sessionKey :: (Generic a, GDatatypeName (Rep a)) => Key
  sessionKey = gDatatypeName $ from (undefined :: a)


  -- | By default Sessions are persisted only to the current page. Set this to `Just []` to make an application-wide Session
  cookiePath :: Maybe Path
  default cookiePath :: Maybe Path
  cookiePath = Nothing


  -- | Encode type to a a cookie value. Defaults to ToJSON
  toCookie :: a -> CookieValue
  default toCookie :: (ToJSON a) => a -> CookieValue
  toCookie = CookieValue . cs . A.encode


  -- | Decode from a cookie value. Defaults to FromJSON
  parseCookie :: CookieValue -> Either Text a
  default parseCookie :: (FromJSON a) => CookieValue -> Either Text a
  parseCookie (CookieValue bs) = do
    first cs $ A.eitherDecodeStrict bs


{- | Persist datatypes in browser cookies. If the session doesn't exist, the 'DefaultParam' is used

@
#EMBED Example/Docs/Sessions.hs data Preferences

#EMBED Example/Docs/Sessions.hs instance DefaultParam Preferences

#EMBED Example/Docs/Sessions.hs page
@
-}
session :: (Session a, Default a, Hyperbole :> es) => Eff es a
session = do
  ms <- lookupSession
  pure $ fromMaybe def ms


-- | Return a session if it exists
lookupSession :: forall a es. (Session a, Hyperbole :> es) => Eff es (Maybe a)
lookupSession = do
  let key = sessionKey @a
  mck <- Cookie.lookup key <$> sessionCookies
  case mck of
    Nothing -> pure Nothing
    Just val -> Just <$> parseSession key val


{- | Persist datatypes in browser cookies

@
#EMBED Example/Docs/Sessions.hs data Preferences

#EMBED Example/Docs/Sessions.hs instance DefaultParam Preferences

#EMBED Example/Docs/Sessions.hs instance HyperView Content
@
-}
saveSession :: forall a es. (Session a, Hyperbole :> es) => a -> Eff es ()
saveSession a = do
  modifyCookies $ Cookie.insert $ sessionCookie a


modifySession :: (Session a, Default a, Hyperbole :> es) => (a -> a) -> Eff es a
modifySession f = do
  s <- session
  let updated = f s
  saveSession updated
  pure updated


modifySession_ :: (Session a, Default a, Hyperbole :> es) => (a -> a) -> Eff es ()
modifySession_ f = do
  _ <- modifySession f
  pure ()


-- | Remove a single 'Session' from the browser cookies
deleteSession :: forall a es. (Session a, Hyperbole :> es) => Eff es ()
deleteSession = do
  let cookie = Cookie (sessionKey @a) (cookiePath @a) Nothing
  modifyCookies $ Cookie.insert cookie


parseSession :: (Session a, Hyperbole :> es) => Key -> CookieValue -> Eff es a
parseSession prm cook = do
  case parseCookie cook of
    Left e -> send $ RespondNow $ Err $ ErrSession prm e
    Right a -> pure a


-- | save a single datatype to a specific key in the session
setCookie :: (ToParam a, Hyperbole :> es) => Cookie -> Eff es ()
setCookie ck = do
  modifyCookies (Cookie.insert ck)


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


sessionCookie :: forall a. (Session a) => a -> Cookie
sessionCookie a =
  Cookie (sessionKey @a) (cookiePath @a) (Just $ toCookie a)


-- | generic datatype name
genericTypeName :: forall a. (Generic a, GDatatypeName (Rep a)) => Text
genericTypeName =
  gDatatypeName $ from (undefined :: a)


class GDatatypeName f where
  gDatatypeName :: f p -> Text


instance (Datatype d) => GDatatypeName (M1 D d f) where
  gDatatypeName _ =
    cs $ datatypeName (undefined :: M1 D d f p)
