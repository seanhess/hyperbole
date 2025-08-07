{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.OAuth2 where

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
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType)
import Network.URI (parseURI)
import Text.Casing (snake)
import Web.Hyperbole.Data.Param (FromParam)
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.GenRandom
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Query
import Web.Hyperbole.Effect.Response (redirect, respondError)
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.Effect.Session (Session (..), deleteSession, lookupSession, saveSession, session)
import Web.Hyperbole.Route


-- TODO: can we redirect back to a specific page? How do we know what the user wants to do?

data OAuth2 :: Effect where
  GenState :: OAuth2 m (Token State)
  GetConfig :: OAuth2 m Config
  ExchangeAuth :: URI -> Token Code -> OAuth2 m Auth


type instance DispatchOf OAuth2 = 'Dynamic


runOAuth2
  :: (GenRandom :> es, IOE :> es)
  => Config
  -> HTTP.Manager
  -> Eff (OAuth2 : es) a
  -> Eff es a
runOAuth2 cfg mgr = interpret $ \_ -> \case
  GenState -> Token <$> genRandomToken 6
  GetConfig -> pure cfg
  ExchangeAuth redUri authCode -> do
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
    case eitherDecode @Auth body of
      Left e -> throwIO $ OAuth2BadResponse e body
      Right tr -> pure tr


-- we prop want to redirect back to the current uri / route, rather than a specific one...
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
logout = deleteSession @Auth


-- | In your redirect handler, call authenticate, then redirect to another page
authenticate :: (Hyperbole :> es, OAuth2 :> es) => Eff es ()
authenticate = do
  c <- authCodeFromRedirect
  authenticateWithCode c


authenticateWithCode :: (Hyperbole :> es, OAuth2 :> es) => Token Code -> Eff es ()
authenticateWithCode code = do
  tok <- exchangeAuth code
  deleteSession @AuthFlow
  saveSession @Auth tok


-- | performed after the redirect, validate the state and return the AuthCode
authCodeFromRedirect :: (Hyperbole :> es) => Eff es (Token Code)
authCodeFromRedirect = do
  err <- lookupParam @Text "error"

  when (isJust err) $ do
    desc <- param "error_description"
    respondError $ ErrAuth desc

  authState <- param @(Token State) "state"
  authCode <- param @(Token Code) "code"
  local <- session @AuthFlow

  unless (local.state == authState) $ do
    respondError $ ErrAuth "Oauth2 State mismatch"

  pure authCode


exchangeAuth :: (Hyperbole :> es, OAuth2 :> es) => Token Code -> Eff es Auth
exchangeAuth authCode = do
  flow <- session @AuthFlow
  send $ ExchangeAuth flow.redirect authCode


lookupAuth :: (Hyperbole :> es) => Eff es (Maybe Auth)
lookupAuth = do
  lookupSession @Auth


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


newtype Token a = Token {value :: Text}
  deriving newtype (FromJSON, ToJSON, FromParam, Eq, Show)


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


data ClientId
data ClientSecret
data Code
data Access
data State
data Local


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


data Auth = Auth
  { tokenType :: TokenType
  , expiresIn :: Maybe Int
  , scope :: Maybe Scopes
  , accessToken :: Token Access
  , refreshToken :: Maybe Text
  }
  deriving (Generic, Show)
instance FromJSON Auth where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = snake}
instance ToJSON Auth where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = snake}
instance Session Auth where
  sessionKey = "OAuth2"
  cookiePath = Just $ Path True []


data OAuth2Error
  = OAuth2BadResponse String BL.ByteString
  | OAuth2TokenRequest HttpException
  | OAuth2BadEnv String String
  deriving (Show, Exception)


-- default tls manager for token requests
initManager :: (IOE :> es) => Eff es HTTP.Manager
initManager = newTlsManager


-- read oauth config from env. This is not required, you can obtain these secrets another way
-- and configure the app however you please. Just pass the results into runOAuth2 in your app
initConfigEnv :: (Environment :> es) => Eff es Config
initConfigEnv = do
  clientId <- Token . cs <$> getEnv "OAUTH2_CLIENT_ID"
  clientSecret <- Token . cs <$> getEnv "OAUTH2_CLIENT_SECRET"
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
