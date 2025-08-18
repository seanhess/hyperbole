module Web.Hyperbole.Server.Socket where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Effectful
import Effectful.Concurrent.Async
import Effectful.Exception
import Network.HTTP.Types as HTTP (parseQuery)
import Network.Wai qualified as Wai
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.URI (URI, path)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Server.Message
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View (View, renderLazyByteString)


data SocketRequest = SocketRequest
  { request :: Maybe Request
  }


handleRequestSocket
  :: (IOE :> es, Concurrent :> es)
  => Wai.Request
  -> Connection
  -> Eff (Hyperbole : es) Response
  -> Eff es ()
handleRequestSocket wreq conn actions = do
  flip catch onMessageError $ do
    msg <- receiveMessage
    req <- parseMessageRequest msg

    void $ async $ do
      res <- trySync $ runHyperbole req actions
      case res of
        -- TODO: catch socket errors separately from SomeException?
        Left (ex :: SomeException) -> do
          -- It's not safe to send any exception over the wire
          -- log it to the console and send the error to the client
          liftIO $ print ex
          res2 <- trySync $ sendError conn (requestMetadata req) (serverError "Internal Server Error")
          case res2 of
            Left e -> liftIO $ putStrLn $ "Socket Error while sending previous error to client: " <> show e
            Right _ -> pure ()
        Right (resp, clnt, rmts) -> do
          let meta = requestMetadata req <> responseMetadata req.path clnt rmts
          case resp of
            (Response _ vw) -> do
              sendUpdateView conn meta vw
            (Err (ErrServer m)) -> sendError conn meta (serverError m)
            (Err err) -> sendError conn meta (serializeError err)
            NotFound -> sendError conn meta (serverError "Not Found")
            (Redirect url) -> sendRedirect conn meta url
 where
  onMessageError :: (IOE :> es) => MessageError -> Eff es a
  onMessageError e = do
    liftIO $ do
      putStrLn "Socket Message Error"
    throwIO e

  errMsg (ErrServer m) = m
  errMsg ErrInternal = "Internal Server Error"
  errMsg e = pack (drop 3 $ show e)

  serializeError (ErrCustom m b) = SerializedError m (renderLazyByteString b)
  serializeError err = serverError $ errMsg err

  receiveMessage :: (IOE :> es) => Eff es Message
  receiveMessage = do
    t <- receiveText conn
    case parseActionMessage t of
      Left e -> throwIO $ InvalidMessage e t
      Right msg -> pure msg

  receiveText :: (IOE :> es) => Connection -> Eff es Text
  receiveText _ = do
    -- c <- ask @Connection
    liftIO $ WS.receiveData conn

  parseMessageRequest :: (IOE :> es) => Message -> Eff es Request
  parseMessageRequest msg =
    case messageRequest msg of
      Left e -> throwIO e
      Right a -> pure a

  messageRequest :: Message -> Either MessageError Request
  messageRequest msg = do
    let pth = path $ cs $ Wai.rawPathInfo wreq
        host = Host $ fromMaybe "" $ L.lookup "Host" headers
        headers = Wai.requestHeaders wreq
        method = "POST"

        body = msg.body.value

    query <- HTTP.parseQuery . cs <$> requireMeta "Query" msg.metadata
    cookie <- cs <$> requireMeta "Cookie" msg.metadata

    cookies <- first (InvalidCookie cookie) <$> Cookie.parse $ Web.Cookie.parseCookies cookie

    pure $
      Request
        { path = pth
        , event = Just msg.event
        , host
        , query
        , body
        , method
        , cookies
        , requestId = msg.requestId
        }
   where
    requireMeta :: MetaKey -> Metadata -> Either MessageError Text
    requireMeta key m =
      maybe (Left $ MissingMeta (cs key)) pure $ lookupMetadata key m


sendUpdateView :: (IOE :> es) => Connection -> Metadata -> View () () -> Eff es ()
sendUpdateView conn meta vw = do
  sendMessage conn meta (RenderedHtml $ renderLazyByteString vw)


sendRedirect :: (IOE :> es) => Connection -> Metadata -> URI -> Eff es ()
sendRedirect conn meta u = do
  sendMessage conn (metaRedirect u <> meta) mempty


sendError :: (IOE :> es) => Connection -> Metadata -> SerializedError -> Eff es ()
sendError conn meta (SerializedError err body) = do
  sendMessage conn (metadata "Error" err <> meta) (RenderedHtml body)


-- low level message. Use sendResponse
sendMessage :: (MonadIO m) => Connection -> Metadata -> RenderedHtml -> m ()
sendMessage conn meta' (RenderedHtml html) = do
  let out = "|UPDATE|\n" <> cs (renderMetadata meta') <> "\n\n" <> html
  liftIO $ WS.sendTextData conn out
