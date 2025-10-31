{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Server.Socket where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar, readTVar, writeTVar)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (throwError_)
import Effectful.Exception
import Effectful.State.Static.Local as Local (get, modify)
import Effectful.Writer.Static.Local (tell)
import Network.HTTP.Types as HTTP (parseQuery)
import Network.Wai qualified as Wai
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.Encoded (Encoded, encodedToText)
import Web.Hyperbole.Data.URI (URI, path, uriToText)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Server.Message
import Web.Hyperbole.Server.Options
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Event (Event (..), TargetViewId (..))
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View (View, addContext, renderLazyByteString)


data SocketRequest = SocketRequest
  { request :: Maybe Request
  }


type RunningActions = Map TargetViewId (Encoded, Async ())


runHyperboleSocket
  :: (IOE :> es)
  => ServerOptions
  -> Connection
  -> Request
  -> Eff (Hyperbole : es) Response
  -> Eff es (Response, Client, [Remote])
runHyperboleSocket _opts conn req = reinterpret (runHyperboleLocal req) $ \_ -> \case
  GetRequest -> do
    pure req
  RespondNow r -> do
    throwError_ r
  PushUpdate vid vw -> do
    sendUpdate conn (targetViewMetadata vid <> requestMetadata req) vw
  GetClient -> do
    Local.get @Client
  ModClient f -> do
    Local.modify @Client f
  TriggerAction vid act -> do
    tell [RemoteAction vid act]
  TriggerEvent name dat -> do
    tell [RemoteEvent name dat]


handleRequestSocket
  :: (IOE :> es, Concurrent :> es)
  => ServerOptions
  -> TVar RunningActions
  -> Wai.Request
  -> Connection
  -> Eff (Hyperbole : es) Response
  -> Eff es ()
handleRequestSocket opts actions wreq conn eff = do
  flip catch onMessageError $ do
    msg <- receiveMessage
    req <- parseMessageRequest msg

    a <- async $ do
      -- is one already running?
      res <- trySync $ runHyperboleSocket opts conn req eff
      case res of
        -- TODO: catch socket errors separately from SomeException?
        Left (ex :: SomeException) -> do
          -- It's not safe to send any exception over the wire
          -- log it to the console and send the error to the client
          liftIO $ print ex
          res2 <- trySync $ sendError conn (requestMetadata req) (opts.serverError ErrInternal)
          case res2 of
            Left e -> liftIO $ putStrLn $ "Socket Error while sending previous error to client: " <> show e
            Right _ -> pure ()
        Right (resp, clnt, rmts) -> do
          let meta = requestMetadata req <> responseMetadata req.path clnt rmts
          case resp of
            (Response _ vw) -> do
              sendResponse conn meta vw
            -- (Err (ErrServer m)) -> sendError conn meta (opts.serverError m)
            (Err err) -> sendError conn meta (opts.serverError err)
            (Redirect url) -> sendRedirect conn meta url

    addRunningAction a req.requestId req.event

    void $ async $ do
      _ <- waitCatch a
      clearRunningAction req.requestId req.event
 where
  addRunningAction :: (IOE :> es, Concurrent :> es) => Async () -> RequestId -> Maybe (Event TargetViewId Encoded) -> Eff es ()
  addRunningAction a (RequestId reqId) = \case
    Nothing -> pure ()
    Just (Event vid act) -> do
      -- liftIO $ putStrLn $ " [add] (" <> cs reqId <> ") " <> cs clientId.value <> "|" <> show vid
      maold <- atomically $ do
        m <- readTVar @RunningActions actions
        writeTVar actions $ M.insert vid (act, a) m
        pure $ M.lookup vid m
      case maold of
        Nothing -> pure ()
        Just (actold, aold) -> do
          liftIO $ putStrLn $ "CANCEL (" <> cs reqId <> ") " <> cs (encodedToText vid.encoded) <> ": " <> cs (encodedToText actold)
          cancel aold

  clearRunningAction :: (IOE :> es, Concurrent :> es) => RequestId -> Maybe (Event TargetViewId Encoded) -> Eff es ()
  clearRunningAction (RequestId _) = \case
    Nothing -> pure ()
    Just (Event vid _) -> do
      _ <- atomically $ modifyTVar actions $ M.delete vid
      pure ()

  onMessageError :: (IOE :> es) => MessageError -> Eff es a
  onMessageError e = do
    liftIO $ do
      putStrLn "Socket Message Error"
    throwIO e

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


sendResponse :: (IOE :> es) => Connection -> Metadata -> View Body () -> Eff es ()
sendResponse conn meta vw = do
  sendMessage "RESPONSE" conn meta (MessageHtml $ renderLazyByteString $ addContext Body vw)


sendUpdate :: (IOE :> es) => Connection -> Metadata -> View Body () -> Eff es ()
sendUpdate conn meta vw = do
  sendMessage "UPDATE" conn meta (MessageHtml $ renderLazyByteString $ addContext Body vw)


sendRedirect :: (IOE :> es) => Connection -> Metadata -> URI -> Eff es ()
sendRedirect conn meta u = do
  sendMessage "REDIRECT" conn meta (MessageText $ uriToText u)


-- TODO: this isn't an UPDATE?
sendError :: (IOE :> es) => Connection -> Metadata -> ServerError -> Eff es ()
sendError conn meta (ServerError err body) = do
  sendMessage "UPDATE" conn (metadata "Error" err <> meta) (MessageHtml $ renderLazyByteString $ addContext Body body)


newtype Command = Command Text
  deriving newtype (IsString)


-- low level message. Use sendResponse
sendMessage :: (MonadIO m) => Command -> Connection -> Metadata -> RenderedMessage -> m ()
sendMessage (Command cmd) conn meta' msg = do
  let header = "|" <> cs cmd <> "|\n" <> cs (renderMetadata meta')

  let body = case msg of
        MessageHtml html -> html
        MessageText t -> cs t

  liftIO $ WS.sendTextData conn (header <> "\n\n" <> body)
