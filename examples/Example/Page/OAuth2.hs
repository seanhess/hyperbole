{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.OAuth2 where

import Data.Aeson (eitherDecode)
import Data.String.Conversions (cs)
import Data.Text (Text, pack, unpack)
import Effectful
import Effectful.Reader.Dynamic
import Example.AppRoute qualified as Route
import Example.Config (AppConfig (..))
import Example.Style qualified as Style
import Example.View.Layout
import Network.HTTP.Client qualified as HTTP
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI (Endpoint (..), Path (..), pathToText)
import Web.Hyperbole.Effect.OAuth2 (Code, OAuth2, Token (..))
import Web.Hyperbole.Effect.OAuth2 qualified as OAuth2
import Web.Hyperbole.Effect.Server.Response (ResponseError (ErrAuth))

--------------------------------------------------------------------------------
-- App Specific Login
--------------------------------------------------------------------------------

-- This code belongs in an application-wide module
-- Using a mock oauth server: https://app.beeceptor.com/mock-server/oauth-mock

data UserSession = UserSession
  { auth :: OAuth2.Authenticated
  , email :: Text
  }
  deriving (Generic, ToJSON, FromJSON)
instance Session UserSession where
  -- we want it to work on any page, not just this one
  cookiePath = Just []

openLogin :: (Hyperbole :> es, OAuth2 :> es, Reader AppConfig :> es) => Eff es a
openLogin = do
  Endpoint appRoot <- (.endpoint) <$> ask @AppConfig
  let redirectUrl = appRoot{uriPath = unpack $ pathToText $ Path True $ routePath Route.OAuth2Authenticate}
  u <- OAuth2.authUrl redirectUrl "email"
  redirect u

logout :: (Hyperbole :> es) => Eff es ()
logout = deleteSession @UserSession

-- | Target of the redirect after the user logs in via OAuth2
handleRedirect :: (Hyperbole :> es, OAuth2 :> es, Reader AppConfig :> es, IOE :> es) => Eff es Response
handleRedirect = do
  authCode <- OAuth2.validateCode
  auth <- OAuth2.exchangeAuth authCode
  info <- fetchUserInfo authCode
  saveSession @UserSession $ UserSession auth info.email
  redirect $ routeUri Route.OAuth2

data GithubUserInfo = GithubUserInfo
  { email :: Text
  }
  deriving (Generic, FromJSON, Show)

-- | Example authenticated request using an oauth access token. in a real app, this should be in an external effect, not IOE
fetchUserInfo :: (IOE :> es, Reader AppConfig :> es, Hyperbole :> es) => Token Code -> Eff es GithubUserInfo
fetchUserInfo (Token authCode) = do
  app <- ask @AppConfig
  req <- HTTP.parseRequest "https://oauth-mock.mock.beeceptor.com/userinfo/github"
  res <- liftIO (HTTP.httpLbs (HTTP.applyBearerAuth (cs authCode) req) app.manager)
  case eitherDecode @GithubUserInfo (HTTP.responseBody res) of
    Left e -> respondError $ ErrAuth $ "Could not parse user info: " <> pack (show e)
    Right info -> do
      liftIO $ putStrLn "GOT"
      liftIO $ print info
      pure info

--------------------------------------------------------------------------------
-- Page / Views
--------------------------------------------------------------------------------

page
  :: (Hyperbole :> es, OAuth2 :> es, Reader AppConfig :> es)
  => Eff es (Page '[Contents])
page = do
  muser <- lookupSession @UserSession
  pure $ exampleLayout Route.OAuth2 $ do
    example "OAuth2" "Example/Page/OAuth2.hs" $ do
      el "Hyperbole provides some helpers to make OAuth2 easier. This is done in 2 steps:"
      el "1. Initiate the login via the OAuth given a redirect url"
      el "2. After the redirect, the library validates the response and fetches an access token from the oauth provider."
      el "The developer can then make authenticated requests, and store a user session"
      col ~ embed $ hyper Contents $ viewContents muser

data Contents = Contents
  deriving (Generic, ViewId)

instance (OAuth2 :> es, Reader AppConfig :> es) => HyperView Contents es where
  data Action Contents
    = Logout
    | Login
    deriving (Generic, ViewAction)

  update Login = do
    openLogin
  update Logout = do
    logout
    pure $ viewContents Nothing

viewContents :: Maybe UserSession -> View Contents ()
viewContents mt = do
  col ~ gap 10 $ do
    maybe viewUnauthorized viewAuthorized mt

viewUnauthorized :: View Contents ()
viewUnauthorized = do
  message "Logged Out!"
  col ~ gap 5 $ do
    button Login "Login" ~ Style.btn

viewAuthorized :: UserSession -> View Contents ()
viewAuthorized user = do
  let auth = user.auth
  message "Successfully Logged In!"
  el ~ pad 5 . grid . gap 10 $ do
    dataItem "Email" user.email
    dataItem "Token Type" $ pack $ show auth.tokenType
    dataItem "Access Token" auth.accessToken.value
    dataItem "Expires In" $ pack $ show auth.expiresIn
    dataItem "Refresh Token" $ pack $ show auth.refreshToken
    dataItem "Scope" $ pack $ show auth.scope
  button Logout "Logout" ~ Style.btn
 where
  dataItem :: Text -> Text -> View c ()
  dataItem lbl cnt = do
    el ~ bold $ do
      text lbl
    el ~ overflow Hidden $ text cnt

  grid :: (Styleable h) => CSS h -> CSS h
  grid =
    utility
      "grid"
      [ "display" :. "grid"
      , "grid-template-columns" :. "max-content auto"
      , "align-items" :. "center"
      ]

message :: View c () -> View c ()
message x = el x ~ pad 10 . border 1
