module Web.Hyperbole.Application
  ( waiApp
  -- , webSocketApplication
  -- , application
  , websocketsOr
  , liveApp
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
import Network.HTTP.Types (Query, parseQuery, status200, status400, status404)
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

liveApp :: (L.ByteString -> L.ByteString) -> Eff '[Hyperbole, IOE] Response -> Eff '[Hyperbole, Reader Connection, IOE] Response -> Wai.Application
liveApp toDoc wai sock =
  websocketsOr
    defaultConnectionOptions
    (socketApp sock)
    (waiApp toDoc wai)


waiApp :: (L.ByteString -> L.ByteString) -> Eff '[Hyperbole, IOE] Response -> Wai.Application
waiApp toDoc actions request respond = do
  req <- fromWaiRequest request
  res <- runEff $ runHyperboleResponse req actions
  respond $ response req res
 where
  -- TODO: logging?
  response :: Request -> Response -> Wai.Response
  response _ (ErrParse e) = respError status400 ("Parse Error: " <> cs e)
  response _ (ErrParam e) = respError status400 $ "ErrParam: " <> cs e
  response _ NotFound = respError status404 "Not Found"
  response req (Response vw) =
    respHtml $
      addDocument req.isFullPage (renderLazyByteString vw)

  respError s = Wai.responseLBS s [contentType ContentText]

  respHtml body =
    let headers = [contentType ContentHtml]
     in Wai.responseLBS status200 headers body

  -- convert to document if full page request. Subsequent POST requests will only include fragments
  addDocument :: Bool -> L.ByteString -> L.ByteString
  addDocument True bd = toDoc bd
  addDocument _ bd = bd


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")


socketApp :: (MonadIO m) => Eff '[Hyperbole, Reader Connection, IOE] Response -> PendingConnection -> m ()
socketApp actions pend = liftIO $ do
  conn <- WS.acceptRequest pend
  forever $ do
    runEff $ runReader conn $ talk actions


talk :: (IOE :> es, Reader Connection :> es) => Eff (Hyperbole : es) Response -> Eff es ()
talk actions = do
  req <- runErrorNoCallStack @SocketError $ request
  case req of
    Left (InvalidMessage e) -> sendError $ "SocketError: " <> e
    Right r -> do
      -- these can't throw a socket error, just a normal response error
      res <- runHyperboleResponse r actions
      case res of
        (Response vw) -> sendView vw
        (ErrParse t) -> sendError $ "ErrParse: " <> t
        (ErrParam t) -> sendError $ "ErrParam: " <> t
        NotFound -> sendError ("NotFound" :: Text)
 where
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

  parseMessage :: Text -> Either SocketError Request
  parseMessage t = do
    (path, query, body) <- messageParts t
    let isFullPage = False
    pure $ Request{path, query, body = cs body, isFullPage}

  messageParts :: Text -> Either SocketError ([Text], Query, Text)
  messageParts t = do
    case T.splitOn "\n" t of
      [url, q, body] -> pure (paths url, query q, body)
      [url, q] -> pure (paths url, query q, "")
      _ -> Left $ InvalidMessage t
   where
    paths p = filter (/= "") $ T.splitOn "/" p
    query q = parseQuery (cs q)

  sendError e = do
    conn <- ask @Connection
    liftIO $ WS.sendTextData conn $ pack (show e)

  sendView vw = do
    conn <- ask @Connection
    liftIO $ WS.sendTextData conn $ renderLazyByteString vw


data SocketError
  = InvalidMessage Text
  deriving (Show, Eq)


fromWaiRequest :: (MonadIO m) => Wai.Request -> m Request
fromWaiRequest wr = do
  body <- liftIO $ Wai.consumeRequestBodyLazy wr
  let isFullPage = Wai.requestMethod wr == "GET"
      path = Wai.pathInfo wr
      query = Wai.queryString wr
  pure $ Request{body, path, query, isFullPage}
