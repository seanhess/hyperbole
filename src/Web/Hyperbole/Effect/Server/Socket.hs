{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.Server.Socket where

import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception (throwIO)
import Effectful.State.Static.Local
import Network.HTTP.Types as HTTP (parseQuery)
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (Path, URI, path, uriToText)
import Web.Hyperbole.Effect.Server.Response
import Web.Hyperbole.Effect.Server.Types
import Web.Hyperbole.View (View, renderLazyByteString)


data SocketRequest = SocketRequest
  { request :: Maybe Request
  }


runServerSockets
  :: (IOE :> es)
  => Connection
  -> Eff (Server : es) a
  -> Eff es a
runServerSockets conn = reinterpret runLocal $ \_ -> \case
  -- load the request
  LoadRequest -> do
    loadRequest
  RemoteAdd n -> do
    -- TODO: send an out-of-band message to the client block, and wait for a response
    --  we can certainly send a message here...
    --  but how do we wait for the response?
    --  1. create a new empty mvar
    --  A. if the server gets a message that's tagged as the other thing, throw it into the MVar
    --  2. send message to client
    --  3. wait for the mvar
    --
    --  I'm worried about leaks, but I guess those can happen regardless
    pure () -- receiveRequest conn
  SendResponse client res -> do
    req <- loadRequest
    -- cannot be called before loading the request!
    case res of
      (Response vid vw) -> do
        sendView req.path conn client vid vw
      (Err (ErrServer m)) -> sendError req.requestId conn (serverError m)
      (Err err) -> sendError req.requestId conn (serializeError err)
      Empty -> sendError req.requestId conn (serverError "Empty")
      NotFound -> sendError req.requestId conn (serverError "Not Found")
      (Redirect url) -> sendRedirect req.path conn client url
 where
  runLocal :: Eff (State SocketRequest : es) a -> Eff es a
  runLocal = evalState (SocketRequest Nothing)

  loadRequest :: (State SocketRequest :> es, IOE :> es) => Eff es Request
  loadRequest = do
    sock <- get @SocketRequest
    case sock.request of
      -- return the existing request, don't wait for another one!
      Just r -> pure r
      -- load the request for the first time
      Nothing -> do
        req <- receiveRequest conn
        put $ SocketRequest (Just req)
        pure req

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

      cookies <- first (InternalSocket . InvalidCookie (cs cook)) <$> Cookie.parse $ Web.Cookie.parseCookies $ cs $ header cook

      pure $ Request{path = pth, host, query, body, method, cookies, requestId}

    -- drop up to the colon, then ': '
    header = T.drop 2 . T.dropWhile (/= ':')


sendView :: (IOE :> es) => Path -> Connection -> Client -> TargetViewId -> View () () -> Eff es ()
sendView reqPath conn client tv vw = do
  sendResponse reqPath conn client (viewIdMeta tv) (renderLazyByteString vw)
 where
  viewIdMeta :: TargetViewId -> Metadata
  viewIdMeta (TargetViewId vid) = Metadata [("VIEW-ID", cs vid)]


sendRedirect :: (IOE :> es) => Path -> Connection -> Client -> URI -> Eff es ()
sendRedirect reqPath conn client u = do
  sendResponse reqPath conn client (metadata "REDIRECT" (uriToText u)) ""


-- send response with client metadata
sendResponse :: (MonadIO m) => Path -> Connection -> Client -> Metadata -> BL.ByteString -> m ()
sendResponse reqPath conn client meta =
  sendMessage conn (responseMeta <> meta)
 where
  responseMeta :: Metadata
  responseMeta =
    metaRequestId client.requestId <> metaSession client.session <> metaQuery client.query

  metaSession :: Cookies -> Metadata
  metaSession cookies = mconcat $ fmap metaCookie $ Cookie.toList cookies
   where
    metaCookie :: Cookie -> Metadata
    metaCookie cookie =
      Metadata [("COOKIE", cs (Cookie.render reqPath cookie))]


sendError :: (IOE :> es) => RequestId -> Connection -> SerializedError -> Eff es ()
sendError reqId conn (SerializedError err body) = do
  sendMessage conn (metaRequestId reqId <> metadata "ERROR" err) body


-- low level message. Use sendResponse
sendMessage :: (MonadIO m) => Connection -> Metadata -> BL.ByteString -> m ()
sendMessage conn meta' cnt = do
  let msg = renderMetadata meta' <> "\n" <> cnt
  liftIO $ WS.sendTextData conn msg
 where
  renderMetadata :: Metadata -> BL.ByteString
  renderMetadata (Metadata m) = BL.intercalate "\n" $ fmap (uncurry metaLine) m

  metaLine :: BL.ByteString -> Text -> BL.ByteString
  metaLine name value = "|" <> name <> "|" <> cs value


-- Metadata --------------------------------------------

metadata :: BL.ByteString -> Text -> Metadata
metadata name value = Metadata [(name, value)]


metaRequestId :: RequestId -> Metadata
metaRequestId (RequestId reqId) =
  Metadata [("REQUEST-ID", cs reqId)]


metaQuery :: Maybe QueryData -> Metadata
metaQuery Nothing = mempty
metaQuery (Just q) =
  Metadata [("QUERY", cs $ QueryData.render q)]
