{-# LANGUAGE UndecidableInstances #-}

module Example.Page.OAuth2 where

import Data.Text (Text, pack)
import Effectful
import Example.AppRoute qualified as Route
import Example.Style qualified as Style
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.OAuth2 (Auth (..), OAuth2)
import Web.Hyperbole.Effect.OAuth2 qualified as OAuth2

page
  :: (Hyperbole :> es, OAuth2 :> es)
  => Eff es (Page '[Contents])
page = do
  tr <- OAuth2.lookupAuth
  pure $ exampleLayout Route.OAuth2 $ do
    example "OAuth2" "Example/Page/OAuth2.hs" $ do
      el "Hyperbole provides some helpers to make OAuth2 easier"
      el "1. Initiate the login via the OAuth provider and provide a redirect url"
      el "2. After the redirect, the library can validate the response and fetch an access token from the oauth provider."
      el "The auth information is then stored in the session and can be used to make authorized requests to the auth provider. You can check the stored session info to check if the user is logged in or not"
      col ~ embed $ hyper Contents $ viewContents tr

-- | Target of the redirect after the user logs in via OAuth2
checkAuth :: (Hyperbole :> es, OAuth2 :> es) => Eff es Response
checkAuth = do
  OAuth2.authenticate
  redirect $ routeUri Route.OAuth2

--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

data Contents = Contents
  deriving (Generic, ViewId)

instance (OAuth2 :> es) => HyperView Contents es where
  data Action Contents
    = Logout
    | Login
    deriving (Generic, ViewAction)

  update Login = do
    OAuth2.login Route.OAuth2Authenticate "email"
  update Logout = do
    OAuth2.logout
    pure $ viewContents Nothing

viewContents :: Maybe Auth -> View Contents ()
viewContents mt = do
  col ~ gap 10 $ do
    maybe viewUnauthorized viewAuthorized mt

viewUnauthorized :: View Contents ()
viewUnauthorized = do
  message "Logged Out!"
  col ~ gap 5 $ do
    button Login "Login" ~ Style.btn

viewAuthorized :: Auth -> View Contents ()
viewAuthorized auth = do
  message "Successfully Logged In!"
  el ~ pad 5 . grid . gap 10 $ do
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
      , -- , "gap" :. "0.5rem 1rem"
        "align-items" :. "center"
      ]

message :: View c () -> View c ()
message x = el x ~ pad 10 . border 1
