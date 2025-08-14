module Web.Hyperbole.Server.Socket where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent.Async
import Effectful.Exception
import Network.HTTP.Types as HTTP (parseQuery)
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (Path, URI, path, uriToText)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Server.Message
import Web.Hyperbole.Server.Types
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View (View, renderLazyByteString)


data SocketRequest = SocketRequest
  { request :: Maybe Request
  }


handleRequestSocket
  :: (IOE :> es, Concurrent :> es)
  => Connection
  -> Eff (Hyperbole : es) Response
  -> Eff es ()
handleRequestSocket conn actions = do
  req <- receiveRequest conn

  -- run handler in the background = async
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
  errMsg (ErrServer m) = m
  errMsg ErrInternal = "Internal Server Error"
  errMsg e = pack (drop 3 $ show e)

  serializeError (ErrCustom m b) = SerializedError m (renderLazyByteString b)
  serializeError err = serverError $ errMsg err

  -- onSocketError :: (IOE :> es) => SocketError -> Eff es Response
  -- onSocketError e = do
  --   let msg = cs $ show e
  --   sendError req conn $ serverError msg
  --   pure $ Err $ ErrServer msg

  receiveRequest :: (IOE :> es) => Connection -> Eff es Request
  receiveRequest _ = do
    t <- receiveText conn
    case parseMessage t of
      Left e -> throwIO e
      Right r -> pure r

  receiveText :: (IOE :> es) => Connection -> Eff es Text
  receiveText _ = do
    -- c <- ask @Connection
    liftIO $ WS.receiveData conn

  parseMessage :: Text -> Either SocketError Request
  parseMessage t = do
    case T.splitOn "\n" t of
      [url, host, cook, reqId, body] -> parseValues url cook host reqId (Just body)
      [url, host, cook, reqId] -> parseValues url cook host reqId Nothing
      _ -> Left $ InvalidMessage t
   where
    parseUrl :: Text -> Either SocketError (Text, Text)
    parseUrl u =
      case T.splitOn "?" u of
        [url, query] -> pure (url, query)
        _ -> Left $ InvalidMessage u

    parseValues :: Text -> Text -> Text -> Text -> Maybe Text -> Either SocketError Request
    parseValues url cook hst reqId mbody = do
      (u, q) <- parseUrl url
      let pth = path u
          query = HTTP.parseQuery (cs q)
          host = Host $ cs $ header hst
          method = "POST"
          body = cs $ fromMaybe "" mbody
          requestId = RequestId $ header reqId
          event = lookupEvent query

      cookies <- first (InternalSocket . InvalidCookie (cs cook)) <$> Cookie.parse $ Web.Cookie.parseCookies $ cs $ header cook

      pure $ Request{path = pth, event, host, query = queryRemoveSystem query, body, method, cookies, requestId}

    -- drop up to the colon, then ': '
    header = T.drop 2 . T.dropWhile (/= ':')


sendUpdateView :: (IOE :> es) => Connection -> Metadata -> View () () -> Eff es ()
sendUpdateView conn meta vw = do
  sendMessage conn meta (RenderedHtml $ renderLazyByteString vw)


sendRedirect :: (IOE :> es) => Connection -> Metadata -> URI -> Eff es ()
sendRedirect conn meta u = do
  sendMessage conn (metaRedirect u <> meta) mempty


sendError :: (IOE :> es) => Connection -> Metadata -> SerializedError -> Eff es ()
sendError conn meta (SerializedError err body) = do
  sendMessage conn (metadata "ERROR" err <> meta) (RenderedHtml body)


-- low level message. Use sendResponse
sendMessage :: (MonadIO m) => Connection -> Metadata -> RenderedHtml -> m ()
sendMessage conn meta' (RenderedHtml html) = do
  let msg = renderMetadata meta' <> "\n" <> html
  liftIO $ WS.sendTextData conn msg
