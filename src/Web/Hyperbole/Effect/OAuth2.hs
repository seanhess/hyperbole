{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.OAuth2 where

import Control.Monad (unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Data.Maybe (isJust)
import Data.String (IsString (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Environment
import Effectful.Exception
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException, Request (..), RequestBody (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType)
import Network.URI (parseURI)
import Web.Hyperbole.Data.Param (FromParam)
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.GenRandom
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Query
import Web.Hyperbole.Effect.Response (redirect, respondError)
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.Effect.Session (Session (..), deleteSession, lookupSession, saveSession, session)
import Web.Hyperbole.Route


data OAuth2 :: Effect where
  GenState :: OAuth2 m State
  GetConfig :: OAuth2 m Config
  FetchAccessToken :: URI -> AuthCode -> OAuth2 m TokenResponse


type instance DispatchOf OAuth2 = 'Dynamic


runOAuth2
  :: (GenRandom :> es, IOE :> es)
  => Config
  -> HTTP.Manager
  -> Eff (OAuth2 : es) a
  -> Eff es a
runOAuth2 cfg mgr = interpret $ \_ -> \case
  GenState -> State <$> genRandomToken 6
  GetConfig -> pure cfg
  FetchAccessToken redUri authCode -> do
    baseReq <- HTTP.requestFromURI cfg.token.uri

    let params = tokenParams cfg.clientId cfg.clientSecret redUri authCode

    let req =
          baseReq
            { method = "POST"
            , requestBody = RequestBodyBS $ renderQuery False params
            , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
            }

    res <- liftIO (HTTP.httpLbs req mgr) `catch` (throwIO . OAuth2TokenRequest)

    let body = HTTP.responseBody res
    case eitherDecode @TokenResponse body of
      Left e -> throwIO $ OAuth2BadResponse e body
      Right tr -> pure tr


-- default tls manager for token requests
initManager :: (IOE :> es) => Eff es HTTP.Manager
initManager = newTlsManager


-- read oauth config from env. This is not required, you can obtain these secrets another way
-- and configure the app however you please. Just pass the results into runOAuth2 in your app
initConfigEnv :: (Environment :> es) => Eff es Config
initConfigEnv = do
  clientId <- ClientId . cs <$> getEnv "OAUTH2_CLIENT_ID"
  clientSecret <- ClientSecret . cs <$> getEnv "OAUTH2_CLIENT_SECRET"
  authorize <- Endpoint <$> getEnvURI "OAUTH2_AUTHORIZE_ENDPOINT"
  token <- Endpoint <$> getEnvURI "OAUTH2_TOKEN_ENDPOINT"
  app <- Endpoint <$> getEnvURI "OAUTH2_APP_ENDPOINT"
  pure $ Config{clientId, clientSecret, authorize, token, app}
 where
  getEnvURI n = do
    str <- getEnv n
    case parseURI str of
      Nothing -> throwIO $ OAuth2BadEnv n str
      Just u -> pure u


newtype ClientId = ClientId Text
newtype ClientSecret = ClientSecret Text
newtype Endpoint a = Endpoint {uri :: URI}


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
newtype State = State {value :: Text}
  deriving newtype (FromJSON, ToJSON, FromParam, Eq, Show)


newtype AuthCode = AuthCode Text
  deriving newtype (FromParam, Show)


newtype AccessToken = AccessToken {value :: Text}
  deriving newtype (FromJSON, ToJSON, Show)


newtype Error = Error Text
  deriving newtype (FromParam)


data Auth
data Token
data Local


data AuthFlow = AuthFlow
  { redirect :: URI
  , state :: State
  }
  deriving (Generic, FromJSON, ToJSON)
instance Session AuthFlow where
  sessionKey = "OAuth2AuthFlow"
  cookiePath = Just $ Path True []
instance Default AuthFlow where
  def = AuthFlow (pathUri (Path False [])) (State mempty)


data Config = Config
  { clientId :: ClientId
  , clientSecret :: ClientSecret
  , authorize :: Endpoint Auth
  , token :: Endpoint Token
  , app :: Endpoint Local
  }


data TokenType
  = Bearer
  deriving (Show)
instance ToJSON TokenType where
  toJSON s = toJSON $ show s
instance FromJSON TokenType where
  parseJSON (String "Bearer") = pure Bearer
  parseJSON val = fail $ "expected TokenType but got " <> show val


data TokenResponse = TokenResponse
  { token_type :: TokenType
  , expires_in :: Maybe Int
  , scope :: Maybe Scopes
  , access_token :: AccessToken
  , refresh_token :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
instance Session TokenResponse where
  sessionKey = "OAuth2TokenResponse"
  cookiePath = Just $ Path True []


login :: (Route r, Hyperbole :> es, OAuth2 :> es) => r -> Scopes -> Eff es a
login redRoute scopes = do
  config <- send GetConfig
  state <- send GenState
  let rp = routePath redRoute
  let red = config.app.uri{uriPath = cs $ pathToText $ Path True rp}
  let url = authorizationUrl config.authorize config.clientId red scopes state
  saveSession $ AuthFlow red state
  redirect url


logout :: (Hyperbole :> es) => Eff es ()
logout = deleteSession @TokenResponse


authenticate :: (Hyperbole :> es, OAuth2 :> es) => Eff es ()
authenticate = do
  c <- authCodeFromRedirect
  authenticateWithCode c


authenticateWithCode :: (Hyperbole :> es, OAuth2 :> es) => AuthCode -> Eff es ()
authenticateWithCode code = do
  tok <- fetchAccessToken code
  deleteSession @AuthFlow
  saveSession @TokenResponse tok


-- | performed after the redirect, validate the state and return the AuthCode
authCodeFromRedirect :: (Hyperbole :> es) => Eff es AuthCode
authCodeFromRedirect = do
  err <- lookupParam @Text "error"

  when (isJust err) $ do
    desc <- param "error_description"
    respondError $ ErrAuth desc

  authState <- param @State "state"
  authCode <- param @AuthCode "code"
  local <- session @AuthFlow

  unless (local.state == authState) $ do
    respondError $ ErrAuth "Oauth2 State mismatch"

  pure authCode


fetchAccessToken :: (Hyperbole :> es, OAuth2 :> es) => AuthCode -> Eff es TokenResponse
fetchAccessToken authCode = do
  flow <- session @AuthFlow
  send $ FetchAccessToken flow.redirect authCode


lookupAuth :: (Hyperbole :> es) => Eff es (Maybe TokenResponse)
lookupAuth = do
  lookupSession @TokenResponse


authorizationUrl :: Endpoint Auth -> ClientId -> URI -> Scopes -> State -> URI
authorizationUrl (Endpoint auth) (ClientId cid) redUrl (Scopes scopes) (State state) =
  auth
    .?. ("response_type", Just "code")
    .?. ("client_id", Just $ cs cid)
    .?. ("redirect_uri", Just $ cs $ uriToText redUrl)
    .?. ("scope", Just $ cs $ T.intercalate " " scopes)
    .?. ("state", Just $ cs state)


tokenParams :: ClientId -> ClientSecret -> URI -> AuthCode -> Query
tokenParams (ClientId cid) (ClientSecret sec) redUrl (AuthCode ac) =
  [ ("grant_type", Just "authorization_code")
  , ("client_id", Just $ cs cid)
  , ("client_secret", Just $ cs sec)
  , ("redirect_uri", Just $ cs $ uriToText redUrl)
  , ("code", Just $ cs ac)
  ]


data OAuth2Error
  = OAuth2BadResponse String BL.ByteString
  | OAuth2TokenRequest HttpException
  | OAuth2BadEnv String String
  deriving (Show, Exception)
