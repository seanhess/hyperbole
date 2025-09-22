{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.OAuth2 where

import Data.Aeson (eitherDecode)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Effectful
import Effectful.Reader.Dynamic
import Example.AppRoute qualified as Route
import Example.Config (AppConfig (..))
import Example.Style.Cyber as Cyber (btn, font)
import Example.View.Layout
import Network.HTTP.Client qualified as HTTP
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI (Endpoint (..), (./.))
import Web.Hyperbole.Effect.OAuth2 (Access, OAuth2, Token (..))
import Web.Hyperbole.Effect.OAuth2 qualified as OAuth2
import Web.Hyperbole.Types.Response (ResponseError (ErrAuth))

--------------------------------------------------------------------------------
-- App Specific Login
--------------------------------------------------------------------------------

-- This code belongs in an application-wide module
-- This example uses a mock OAuth2 server: https://app.beeceptor.com/mock-server/oauth-mock

data UserSession = UserSession
  { auth :: OAuth2.Authenticated
  , email :: Text
  }
  deriving (Generic, ToEncoded, FromEncoded)
instance Session UserSession where
  -- we want it to work on any page, not just this one
  cookiePath = Just []

openLogin :: (Hyperbole :> es, OAuth2 :> es, Reader AppConfig :> es) => Eff es a
openLogin = do
  Endpoint appRoot <- (.endpoint) <$> ask @AppConfig
  let redirectUrl = appRoot ./. routePath Route.OAuth2Authenticate
  u <- OAuth2.authUrl redirectUrl "email"
  redirect u

logout :: (Hyperbole :> es) => Eff es ()
logout = deleteSession @UserSession

-- | Target of the redirect after the user logs in via OAuth2
handleRedirect :: (Hyperbole :> es, OAuth2 :> es, Reader AppConfig :> es, IOE :> es) => Eff es Response
handleRedirect = do
  authCode <- OAuth2.validateCode
  auth <- OAuth2.exchangeAuth authCode
  info <- fetchUserInfo auth.accessToken
  saveSession @UserSession $ UserSession auth info.email
  redirect $ routeUri Route.OAuth2

data GithubUserInfo = GithubUserInfo
  { email :: Text
  }
  deriving (Generic, FromJSON, Show)

-- | Example authenticated request using an oauth access token. in a real app, this should be in an external effect, not IOE
fetchUserInfo :: (IOE :> es, Reader AppConfig :> es, Hyperbole :> es) => Token Access -> Eff es GithubUserInfo
fetchUserInfo (Token accessTok) = do
  app <- ask @AppConfig
  req <- HTTP.parseRequest "https://oauth-mock.mock.beeceptor.com/userinfo/github"
  res <- liftIO (HTTP.httpLbs (HTTP.applyBearerAuth (cs accessTok) req) app.manager)
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
  => Page es '[Contents]
page = do
  muser <- lookupSession @UserSession
  pure $ exampleLayout Route.OAuth2 $ do
    example Route.OAuth2 $ do
      el "Hyperbole provides some helpers to make OAuth2 easier. This is done in 2 steps:"
      el "1. Initiate the login via the OAuth provider given a redirect url"
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
    button Login "Login" ~ btn

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
  button Logout "Logout" ~ btn
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
message x = el x ~ pad 10 . Cyber.font . border 1
