{-# LANGUAGE UndecidableInstances #-}

module Example.Page.OAuth2
  ( page
  , OAuth2PageEnv
  , getOAuth2PageEnv
  ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (replicateM, when)
import Control.Monad.Except (runExceptT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.URI (parseURI)
import System.Environment (getEnv)
import System.Random (randomRIO)

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Example.AppRoute qualified as Route
import Example.Style qualified as Style
import Network.HTTP.Client qualified as HTTP
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
  { duUsername :: Text
  , duPassword :: Text
  }

data OAuth2PageEnv = OAuth2PageEnv
  { opeOAuth2Config :: OAuth2.OAuth2
  , opeDummyUser :: DummyUser
  , opeHTTPManager :: HTTP.Manager
  }

-- TODO: Add expiry to this state
data RandomState = RandomState Text
  deriving (Generic, Show, ToJSON, FromJSON, Session)

data Token = Token OAuth2.OAuth2Token
  deriving (Generic, Show, ToJSON, FromJSON, Session)

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

getEnvText :: String -> IO Text
getEnvText = fmap T.pack . getEnv

getOAuth2Config :: IO OAuth2.OAuth2
getOAuth2Config =
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

getOAuth2PageEnv :: HTTP.Manager -> IO OAuth2PageEnv
getOAuth2PageEnv httpManager =
  OAuth2PageEnv <$> getOAuth2Config <*> getDummyUser <*> pure httpManager

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
  (Reader OAuth2PageEnv :> es, Hyperbole :> es, IOE :> es) => Eff es a
redirectToAuthServer = do
  randomState <- replicateM 15 (randomRIO ('a', 'z'))
  conf <- ask @OAuth2PageEnv
  saveSession (RandomState (T.pack randomState))
  redirect $ toURI $
    OAuth2.authorizationUrlWithParams
      [ ("scope", "email")
      , ("state", BS.pack randomState)
      ]
      conf.opeOAuth2Config

-- NOTE:
--
-- 1. @lookupParam @String "state"@ does not work. This is because of how
-- `FromParam` is implemented on a "[Char]" type. Although the behaviour seems
-- reasonable, it was confusing.
--
-- 2. Using "deleteParam" in "setup" does not work, but adding "deleteParam" in
-- the "update" works.
--
lookupExchangeToken :: (Hyperbole :> es) => Eff es (Maybe OAuth2.ExchangeToken)
lookupExchangeToken = do
  mSavedState <- lookupSession @RandomState
  mRespState <- lookupParam "state"
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

instance (Reader OAuth2PageEnv :> es, IOE :> es) => HyperView Contents es where
  data Action Contents
    = Login
    | Logout
    deriving (Generic, ViewAction)
  update Login = redirectToAuthServer
  update Logout = do
    deleteParam "code"
    deleteParam "state"
    deleteSession @Token
    conf <- ask @OAuth2PageEnv
    pure $ viewContent $ Unauthorized conf.opeDummyUser

--------------------------------------------------------------------------------
-- View Utils
--------------------------------------------------------------------------------

data ViewState
  = Unauthorized DummyUser
  | Authorized OAuth2.OAuth2Token

message :: View c () -> View c ()
message x = el x ~ pad 10 . border 1

formatToken :: OAuth2.OAuth2Token -> [(Text, Text)]
formatToken tok =
  [ ("Access Token", shorten $ OAuth2.atoken $ OAuth2.accessToken tok)
  , ("Token Type", maybe "None" id $ OAuth2.tokenType tok)
  , ("Expires In", maybe "None" ((<> " seconds") . T.pack . show) $ OAuth2.expiresIn tok)
  , ("Refresh Token", maybe "None" OAuth2.rtoken $ OAuth2.refreshToken tok)
  , ("Scope", maybe "None" id $ OAuth2.scope tok)
  ]
  where
  shorten t = if T.length t > 40 then T.take 40 t <> "..." else t

-- | Render a table for a single token
renderTokenTable :: OAuth2.OAuth2Token -> View c ()
renderTokenTable tok = table (formatToken tok) $ do
  tcol (th "Field" ~ textAlign AlignLeft) (td . text . fst)
  tcol (th "Value" ~ textAlign AlignLeft) (td . text . snd)

-- NOTE: Moving "col ~ gap 15" right after "hyper Contents" like so:
-- @
-- hyper Contents $ col ~ gap 15 $ viewContent ...
-- @
-- has unintended behaviour. "col ~ gap 15" for some reason does not have an
-- effect once the user logs in and then logs out.
--
-- It looks like everything after "hyper Contents" is replaced on a change. So
-- the behaviour described above makes sense in that case and I have misused the
-- view creation.
--
viewContent :: ViewState -> View Contents ()
viewContent (Unauthorized du) = col ~ gap 15 $ unauthorizedContent du
viewContent (Authorized tok) = col ~ gap 15 $ authorizedContent tok

unauthorizedContent :: DummyUser -> View Contents ()
unauthorizedContent du = do
  message "Logged Out!"
  col ~ gap 5 $ do
    tag "fieldset" ~ pad 10 . border 1 $ do
      tag "legend" $ text "Dummy User Information"
      row ~ gap 5 $ el "Username:" >> el (text du.duUsername) ~ bold
      row ~ gap 5 $ el "Password:" >> el (text du.duPassword) ~ bold
  col ~ gap 5 $ do
    el "Please click on the button below to Login:"
    button Login "Login" ~ Style.btn

authorizedContent :: OAuth2.OAuth2Token -> View Contents ()
authorizedContent tok = do
  message "Successfully Logged In!"
  renderTokenTable tok
  button Logout "Logout" ~ Style.btn

--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------

setup ::
  (Hyperbole :> es, Reader OAuth2PageEnv :> es, IOE :> es) => Eff es ViewState
setup = do
  mtoken <- lookupSession @Token
  case mtoken of
    Just (Token tok) -> pure $ Authorized tok
    Nothing -> do
      mExchangeTok <- lookupExchangeToken
      conf <- ask @OAuth2PageEnv
      case mExchangeTok of
        Nothing -> setupUnauthorizedState conf.opeDummyUser
        Just etok -> setupAuthorizedState etok conf
  where
  setupUnauthorizedState du = pure $ Unauthorized du
  setupAuthorizedState etok conf = do
    res <-
      runExceptT $
        OAuth2.fetchAccessToken conf.opeHTTPManager conf.opeOAuth2Config etok
    case res of
      Left err -> error $ show err
      Right oauthTok -> do
        saveSession (Token oauthTok)
        pure $ Authorized oauthTok

page ::
  (Hyperbole :> es, Reader OAuth2PageEnv :> es, IOE :> es) =>
  Eff es (Page '[Contents])
page = do
  vs <- setup
  pure $ exampleLayout Route.OAuth2 $ do
    example "OAuth2" "Example/Page/OAuth2.hs" $ do
      col ~ embed $ hyper Contents $ viewContent vs
