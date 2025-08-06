{-# LANGUAGE UndecidableInstances #-}

module Example.Page.OAuth2 where

import Control.Exception (Exception, throwIO)
import Control.Monad (replicateM, unless, when)
import Control.Monad.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Debug.Trace
import Effectful
import Effectful.Reader.Dynamic
import Example.AppRoute qualified as Route
import Example.Style qualified as Style
import Example.View.Layout
import Network.HTTP.Client qualified as HTTP
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.URI (parseURI)
import System.Environment (getEnv)
import System.Random (randomRIO)
import URI.ByteString qualified as URI
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.GenRandom
import Web.Hyperbole.Effect.OAuth2 (OAuth2, Scopes (..), TokenResponse (..))
import Web.Hyperbole.Effect.OAuth2 qualified as OAuth2
import Web.Hyperbole.Effect.Server.Response (ResponseError (..))

--------------------------------------------------------------------------------
-- LIB
--------------------------------------------------------------------------------

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

checkAuth
  :: (Hyperbole :> es, OAuth2 :> es)
  => Eff es Response
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

viewContents :: Maybe TokenResponse -> View Contents ()
viewContents mt = do
  col ~ gap 10 $ do
    maybe viewUnauthorized viewAuthorized mt

viewUnauthorized :: View Contents ()
viewUnauthorized = do
  message "Logged Out!"
  col ~ gap 5 $ do
    button Login "Login" ~ Style.btn

viewAuthorized :: TokenResponse -> View Contents ()
viewAuthorized tok = do
  message "Successfully Logged In!"
  el ~ pad 5 . grid . gap 10 $ do
    dataItem "Token Type" $ pack $ show tok.token_type
    dataItem "Access Token" $ tok.access_token.value
    dataItem "Expires In" $ pack $ show tok.expires_in
    dataItem "Refresh Token" $ pack $ show tok.refresh_token
    dataItem "Scope" $ pack $ show tok.scope
  button Logout "Logout" ~ Style.btn
 where
  dataItem :: Text -> Text -> View c ()
  dataItem lbl cnt = do
    el ~ bold $ do
      text lbl
    el ~ overflow Hidden $ text cnt

  shorten t = if T.length t > 40 then T.take 40 t <> "..." else t

  grid :: (Styleable h) => CSS h -> CSS h
  grid =
    utility
      "grid"
      [ "display" :. "grid"
      , "grid-template-columns" :. "max-content auto"
      -- , "gap" :. "0.5rem 1rem"
      , "align-items" :. "center"
      ]

message :: View c () -> View c ()
message x = el x ~ pad 10 . border 1
