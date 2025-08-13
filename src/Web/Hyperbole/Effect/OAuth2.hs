{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Web.Hyperbole.Effect.OAuth2
  ( OAuth2 (..)
  , authUrl
  , validateCode
  , exchangeAuth
  , exchangeRefresh
  , runOAuth2
  , getConfigEnv
  , Scopes (..)
  , AuthFlow (..)
  , Config (..)
  , TokenType (..)
  , Authenticated (..)
  , Token (..)
  , ClientId
  , ClientSecret
  , Code
  , Access
  , Refresh
  , State
  , Auth
  , OAuth2Error (..)
  ) where

import Control.Monad (unless, when)
import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), Value (..), defaultOptions, eitherDecode, genericParseJSON, genericToJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Data.Maybe (isJust)
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Environment
import Effectful.Exception
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException, Request (..), RequestBody (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hAccept, hContentType)
import Network.URI (parseURI)
import Text.Casing (quietSnake)
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.GenRandom
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Query
import Web.Hyperbole.Effect.Response (respondError)
import Web.Hyperbole.Effect.Session (Session (..), deleteSession, saveSession, session)
import Web.Hyperbole.Types.Response


-- TODO: could all hyperbole applications have an oauth2 redirect endpoint?
-- TVar (Map State (Eff es _))
-- then you could execute it afterwards!

-- High level effect interface

authUrl :: (OAuth2 :> es) => URI -> Scopes -> Eff es URI
authUrl redirectUrl scopes = send $ AuthUrl redirectUrl scopes


validateCode :: (OAuth2 :> es) => Eff es (Token Code)
validateCode = send ValidateCode


exchangeAuth :: (OAuth2 :> es) => Token Code -> Eff es Authenticated
exchangeAuth authCode = send $ ExchangeAuth authCode


exchangeRefresh :: (OAuth2 :> es) => Token Refresh -> Eff es Authenticated
exchangeRefresh refToken = send $ ExchangeRefresh refToken


data OAuth2 :: Effect where
  AuthUrl :: URI -> Scopes -> OAuth2 m URI
  ValidateCode :: OAuth2 m (Token Code)
  ExchangeAuth :: Token Code -> OAuth2 m Authenticated
  ExchangeRefresh :: Token Refresh -> OAuth2 m Authenticated


type instance DispatchOf OAuth2 = 'Dynamic


runOAuth2
  :: (GenRandom :> es, IOE :> es, Hyperbole :> es)
  => Config
  -> HTTP.Manager
  -> Eff (OAuth2 : es) a
  -> Eff es a
runOAuth2 cfg mgr = interpret $ \_ -> \case
  AuthUrl red scopes -> do
    state <- genRandomToken 6
    let url = authorizationUrl cfg.authorize cfg.clientId red scopes state
    saveSession $ AuthFlow red state
    pure url
  ValidateCode -> do
    flow <- session @AuthFlow
    validateRedirectParams flow
  ExchangeAuth authCode -> do
    flow <- session @AuthFlow
    let params = tokenParams cfg.clientId cfg.clientSecret flow.redirect authCode
    auth <- sendTokenRequest cfg mgr params
    deleteSession @AuthFlow
    pure auth
  ExchangeRefresh refToken -> do
    let params = refreshParams cfg.clientId cfg.clientSecret refToken
    sendTokenRequest cfg mgr params


{- | read oauth config from env. This is not required, you can obtain these secrets another way
and configure the app however you please. Just pass the results into runOAuth2 in your app
-}
getConfigEnv :: (Environment :> es) => Eff es Config
getConfigEnv = do
  clientId <- Token . cs <$> getEnv "OAUTH2_CLIENT_ID"
  clientSecret <- Token . cs <$> getEnv "OAUTH2_CLIENT_SECRET"
  authorize <- Endpoint <$> getEnvURI "OAUTH2_AUTHORIZE_ENDPOINT"
  token <- Endpoint <$> getEnvURI "OAUTH2_TOKEN_ENDPOINT"
  pure $ Config{clientId, clientSecret, authorize, token}
 where
  getEnvURI n = do
    str <- getEnv n
    case parseURI str of
      Nothing -> throwIO $ OAuth2BadEnv n str
      Just u -> pure u


-- Types -------------------------------------------------

newtype Scopes = Scopes [Text]
  deriving (Show)
instance ToJSON Scopes where
  toJSON (Scopes ss) = String $ T.unwords ss
instance FromJSON Scopes where
  parseJSON v = do
    t <- parseJSON @String v
    pure $ fromString t
instance IsString Scopes where
  fromString s = Scopes $ T.words $ cs s


data ClientId
data ClientSecret
data Code
data Refresh
data Access
data State
data Auth

#if (!MIN_VERSION_aeson(2,2,0))
instance FromJSON URI
instance ToJSON URI
instance FromJSON URIAuth
instance ToJSON URIAuth
#endif

data AuthFlow = AuthFlow
  { redirect :: URI
  , state :: Token State
  }
  deriving (Generic, FromJSON, ToJSON)
instance Session AuthFlow where
  sessionKey = "OAuth2AuthFlow"
  cookiePath = Just $ Path True []
instance Default AuthFlow where
  def = AuthFlow (pathUri (Path False [])) (Token mempty)


data Config = Config
  { clientId :: Token ClientId
  , clientSecret :: Token ClientSecret
  , authorize :: Endpoint Auth
  , token :: Endpoint (Token ())
  }


data TokenType
  = Bearer
  deriving (Show)
instance ToJSON TokenType where
  toJSON s = toJSON $ show s
instance FromJSON TokenType where
  parseJSON (String ttyp) | T.toLower ttyp == "bearer" = pure Bearer
  parseJSON val = fail $ "expected TokenType but got " <> show val


data Authenticated = Authenticated
  { tokenType :: TokenType
  , expiresIn :: Maybe Int
  , scope :: Maybe Scopes
  , accessToken :: Token Access
  , refreshToken :: Maybe (Token Refresh)
  }
  deriving (Generic, Show)
instance FromJSON Authenticated where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = quietSnake}
instance ToJSON Authenticated where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = quietSnake}
instance Session Authenticated where
  sessionKey = "OAuth2Authenticated"
  cookiePath = Just $ Path True []


data OAuth2Error
  = OAuth2BadResponse String BL.ByteString
  | OAuth2TokenRequest HttpException
  | OAuth2BadEnv String String
  deriving (Show, Exception)


-- Lower level --------------------------------------------------

authorizationUrl :: Endpoint Auth -> Token ClientId -> URI -> Scopes -> Token State -> URI
authorizationUrl (Endpoint auth) (Token cid) redUrl (Scopes scopes) (Token state) =
  auth{uriQuery = cs $ renderQuery True authParams}
 where
  authParams =
    [ ("response_type", Just "code")
    , ("client_id", Just $ cs cid)
    , ("redirect_uri", Just $ cs $ uriToText redUrl)
    , ("scope", Just $ cs $ T.intercalate " " scopes)
    , ("state", Just $ cs state)
    ]


tokenParams :: Token ClientId -> Token ClientSecret -> URI -> Token Code -> Query
tokenParams (Token cid) (Token sec) redUrl (Token ac) =
  [ ("grant_type", Just "authorization_code")
  , ("client_id", Just $ cs cid)
  , ("client_secret", Just $ cs sec)
  , ("redirect_uri", Just $ cs $ uriToText redUrl)
  , ("code", Just $ cs ac)
  ]


refreshParams :: Token ClientId -> Token ClientSecret -> Token Refresh -> Query
refreshParams (Token cid) (Token sec) (Token ref) =
  [ ("grant_type", Just "refresh_token")
  , ("client_id", Just $ cs cid)
  , ("client_secret", Just $ cs sec)
  , ("refresh_token", Just $ cs ref)
  ]


validateRedirectParams :: (Hyperbole :> es) => AuthFlow -> Eff es (Token Code)
validateRedirectParams flow = do
  err <- lookupParam @Text "error"

  when (isJust err) $ do
    desc <- param "error_description"
    respondError $ ErrAuth desc

  authState <- param @(Token State) "state"
  authCode <- param @(Token Code) "code"

  unless (flow.state == authState) $ do
    respondError $ ErrAuth "Oauth2 State mismatch"

  pure authCode


sendTokenRequest :: (IOE :> es) => Config -> HTTP.Manager -> Query -> Eff es Authenticated
sendTokenRequest cfg mgr params = do
  baseReq <- HTTP.requestFromURI cfg.token.uri

  let req =
        baseReq
          { method = "POST"
          , requestBody = RequestBodyBS $ renderQuery False params
          , requestHeaders =
              [ (hContentType, "application/x-www-form-urlencoded")
              , (hAccept, "application/json")
              ]
          }

  res <- liftIO (HTTP.httpLbs req mgr) `catch` (throwIO . OAuth2TokenRequest)

  let body = HTTP.responseBody res
  case eitherDecode @Authenticated body of
    Left e -> throwIO $ OAuth2BadResponse e body
    Right tr -> pure tr
