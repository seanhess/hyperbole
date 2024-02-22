module Web.Hyperbole.Application
  ( waiApplication
  -- , webSocketApplication
  -- , application
  , websocketsOr
  , fromWaiRequest
  , talk
  , socketApp
  ) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Network.HTTP.Types (Method, Query, parseQuery, status200, status400, status404)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Hyperbole.Effect
import Web.Hyperbole.Route
import Web.View (View, renderLazyByteString)


{- | Start both a websockets and a WAI server. Wai app serves initial pages, and attempt to process actions via sockets
  If the socket connection is unavailable, will fall back to the WAI app to process actions
-}

-- you can't pass this in once, because it is used twice
-- this doesn't work with ALL (Hyperbole : es). It ONLY works with [Hyperbole, IOE]
-- we are missing the IOE?
-- application :: (Route route) => (L.ByteString -> L.ByteString) -> (route -> Eff (Hyperbole : es) Response) -> Wai.Application
-- application toDoc actions request respond = do
--   resp <- runEff $ waiApplication toDoc actions request
--   respond resp

-- websocketsOr
--   defaultConnectionOptions
--   (socketApplication)
--   (waiApplication toDoc actions)

-- -- socketApplication :: (Route route, IOE :> es) => (route -> Eff (Hyperbole : es) Response) -> PendingConnection -> Eff es ()
-- socketApplication :: PendingConnection -> IO ()
-- socketApplication pending = do
--   conn <- liftIO $ WS.acceptRequest pending
--   forever $ do
--     res <- runEff . runReader conn . runErrorNoCallStack @SocketError $ talk actions
--     case res of
--       Left err -> fail "OH NO"
--       Right _ -> pure ()

-- runSocket = runEff . runReader conn . runErrorNoCallStack @SocketError

waiApplication :: forall es. (IOE :> es) => (L.ByteString -> L.ByteString) -> Eff (Hyperbole : es) Response -> Request -> Eff es Wai.Response
waiApplication toDoc actions req = do
  res <- runHyperbole req actions
  sendResponse $ either id id res
 where
  -- TODO: logging?
  sendResponse :: Response -> Eff es Wai.Response
  sendResponse (ErrParse e) = respError status400 ("Parse Error: " <> cs e)
  sendResponse (ErrParam e) = respError status400 $ "ErrParam: " <> cs e
  sendResponse NotFound = respError status404 "Not Found"
  sendResponse (Response vw) =
    respHtml $
      addDocument req.method (renderLazyByteString vw)

  respError s e =
    pure $ Wai.responseLBS s [contentType ContentText] e

  respHtml body = do
    let headers = [contentType ContentHtml]
    pure $ Wai.responseLBS status200 headers body

  -- convert to document if GET. Subsequent POST requests will only include fragments
  addDocument :: Maybe Method -> L.ByteString -> L.ByteString
  addDocument (Just "GET") bd = toDoc bd
  addDocument _ bd = bd


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")


-- TODO: remove IOE and switch back to MonadIO
socketApp :: (MonadIO m) => PendingConnection -> Eff '[Hyperbole, Reader Connection, IOE] Response -> m ()
socketApp pend actions = liftIO $ do
  conn <- WS.acceptRequest pend
  forever $ do
    runEff $ runReader conn $ talk actions


talk :: (IOE :> es, Reader Connection :> es) => Eff (Hyperbole : es) Response -> Eff es ()
talk actions = do
  req <- runErrorNoCallStack @SocketError $ request

  case req of
    Left (InvalidMessage e) -> sendError $ "SocketError: " <> e
    Right r -> do
      (res :: Either Response Response) <- runHyperbole r actions

      case either id id res of
        (Response vw) -> sendView vw
        (ErrParse t) -> sendError $ "ErrParse: " <> t
        (ErrParam t) -> sendError $ "ErrParam: " <> t
        NotFound -> sendError ("NotFound" :: Text)
 where
  -- Left err -> _ -- sendError err

  -- runSocket :: Eff '[Error SocketError, Reader Connection, IOE] Response -> IO (Either SocketError Response)
  -- runSocket = runEff . runReader conn . runErrorNoCallStack @SocketError

  -- runSocket :: Eff '[Error SocketError, Reader Connection, IOE] Response -> IO (Either SocketError Response)
  -- runSocket = runReader conn . runErrorNoCallStack @SocketError

  request :: (IOE :> es, Reader Connection :> es, Error SocketError :> es) => Eff es Request
  request = do
    t <- receive
    case parseMessage t of
      Left e -> throwError e
      Right r -> pure r

  receive :: (Reader Connection :> es, IOE :> es) => Eff es Text
  receive = do
    c <- ask @Connection
    liftIO $ WS.receiveData c

  -- We are constructing a request here from the message! It includes the original URI,
  -- could we argue it should set cookies, etc?
  -- can't set cookies directly, but what CAN we do?
  -- sure, we are just abstracting a basic browser call/response cycle here
  -- make the socket connection act the same
  -- we don't need the same protocol, but still
  parseMessage :: Text -> Either SocketError Request
  parseMessage t = do
    (path, query, body) <- messageParts t
    pure $ Request path query (cs body) Nothing

  messageParts :: Text -> Either SocketError ([Text], Query, Text)
  messageParts t = do
    case T.splitOn "\n" t of
      [url, q, body] -> pure (paths url, query q, body)
      [url, q] -> pure (paths url, query q, "")
      _ -> Left $ InvalidMessage t
   where
    paths p = filter (/= "") $ T.splitOn "/" p
    query q = parseQuery (cs q)

  sendView vw = do
    conn <- ask @Connection
    liftIO $ WS.sendTextData conn $ renderLazyByteString vw

  sendError e = do
    conn <- ask @Connection
    liftIO $ WS.sendTextData conn $ pack (show e)


data SocketError
  = InvalidMessage Text
  deriving (Show, Eq)


fromWaiRequest :: (MonadIO m) => Wai.Request -> m Request
fromWaiRequest wr = do
  bd <- liftIO $ Wai.consumeRequestBodyLazy wr
  pure $ Request (Wai.pathInfo wr) (Wai.queryString wr) bd (Just $ Wai.requestMethod wr)
