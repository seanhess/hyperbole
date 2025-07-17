{-# LANGUAGE UndecidableInstances #-}

module Example.Page.OAuth2
  ( OAuth2.OAuth2
  , page
  , getOAuth2
  ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (replicateM, when)
import Data.Maybe (fromMaybe)
import Data.Text (pack, Text)
import Network.URI (parseURI)
import System.Environment (getEnv)
import System.Random (randomRIO)

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Example.AppRoute qualified as Route
import Network.OAuth.OAuth2 qualified as OAuth2
import URI.ByteString qualified as URI

import Effectful
import Effectful.Reader.Dynamic
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data DummyUser = DummyUser
  { username :: Text
  , password :: Text
  }

-- TODO: Add expiry to this state
data RandomState = RandomState String
  deriving (Generic, Show, ToJSON, FromJSON, Session)

data Token = Token OAuth2.OAuth2Token
  deriving (Generic, Show, ToJSON, FromJSON, Session)

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

getEnvText :: String -> IO Text
getEnvText = fmap pack . getEnv

getOAuth2 :: IO OAuth2.OAuth2
getOAuth2 =
  OAuth2.OAuth2
    <$> getEnvText "OAUTH2_CLIENT_ID"
    <*> getEnvText "OAUTH2_CLIENT_SECRET"
    <*> getEnvURIRef "OAUTH2_AUTHORIZE_ENDPOINT"
    <*> getEnvURIRef "OAUTH2_TOKEN_ENDPOINT"
    <*> getEnvURIRef "OAUTH2_REDIRECT_URI"
  where
  getEnvURIRef var = do
    res <- BS.pack <$> getEnv var
    case URI.parseURI URI.strictURIParserOptions res of
      Left err -> error $ "getEnvURIRef: " ++ show err
      Right val -> pure val

getDummyUser :: IO DummyUser
getDummyUser = do
  DummyUser
    <$> getEnvText "OAUTH2_DUMMY_USERNAME"
    <*> getEnvText "OAUTH2_DUMMY_PASSWORD"

--------------------------------------------------------------------------------
-- Authentication
--------------------------------------------------------------------------------

-- Convert URIRef Absolute (uri-bytestring) to URI (network-uri)
--
-- NOTE: It is more efficient to construct the URI record but for the time being
-- we'll do things in a simple and stupid way.
toURI :: URI.URIRef URI.Absolute -> URI
toURI uriRef =
  fromMaybe (error "toURI: Unable to parse URI")
    $ parseURI
    $ BS.unpack
    $ BSL.toStrict
    $ BB.toLazyByteString
    $ URI.serializeURIRef uriRef

redirectToAuthServer ::
  (Reader (OAuth2.OAuth2) :> es, Hyperbole :> es, IOE :> es) => Eff es a
redirectToAuthServer = do
  randomState <- replicateM 15 (randomRIO ('a', 'z'))
  oauth2Obj <- ask
  saveSession (RandomState randomState)
  redirect $ toURI $
    OAuth2.authorizationUrlWithParams
      [ ("scope", "photo+offline_access")
      , ("state", BS.pack randomState)
      ]
      oauth2Obj

lookupExchangeToken :: (Hyperbole :> es) => Eff es (Maybe OAuth2.ExchangeToken)
lookupExchangeToken = do
  mRespState <- lookupParam "state"
  mSavedState <- lookupSession @RandomState
  mCode <- lookupParam "code"
  pure $ do
    respState <- mRespState
    (RandomState savedState) <- mSavedState
    when (respState /= savedState)
      $ error "parseExchangeToken: The state does not match"
    code_ <- mCode
    pure $ OAuth2.ExchangeToken code_

--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

data Contents = Contents
  deriving (Generic, ViewId)

instance (Reader (OAuth2.OAuth2) :> es, IOE :> es) => HyperView Contents es where
  data Action Contents
    = Login
    | Logout
    deriving (Generic, ViewAction)
  update Login = redirectToAuthServer
  update Logout = do
    deleteSession @Token
    du <- liftIO getDummyUser
    pure $ viewContent $ Unauthorized du

--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------

data ViewState = Authorized OAuth2.OAuth2Token | Unauthorized DummyUser

setup :: (Hyperbole :> es, IOE :> es) => Eff es ViewState
setup = do
  mtoken <- lookupSession @Token
  case mtoken of
    Just (Token tok) -> pure $ Authorized tok
    Nothing -> do
      mExchangeTok <- lookupExchangeToken
      case mExchangeTok of
        Nothing -> do
          du <- liftIO getDummyUser
          pure $ Unauthorized du
        Just exchangeTok -> do
          oauthTok <- error "Unimplemented"
          saveSession (Token oauthTok)
          pure $ Authorized oauthTok

page :: (Hyperbole :> es, IOE :> es) => Eff es (Page '[Contents])
page = do
  vs <- setup
  pure $ exampleLayout Route.OAuth2 $ do
    example "OAuth2" "Example/Page/OAuth2.hs" $ do
      col ~ embed $ hyper Contents $ viewContent vs

viewContent :: ViewState -> View Contents ()
viewContent (Unauthorized du) = unauthorizedContent du
viewContent (Authorized tok) = authorizedContent tok

unauthorizedContent :: DummyUser -> View Contents ()
unauthorizedContent _ = do
  el "Unauthorized content"

authorizedContent :: OAuth2.OAuth2Token -> View Contents ()
authorizedContent _ =
  el "Authorized content"
