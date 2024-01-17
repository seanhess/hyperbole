module Web.Hyperbole.Application
  ( waiApplication
  -- , webSocketApplication
  , application
  , websocketsOr
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
application :: (Route route) => (L.ByteString -> L.ByteString) -> (route -> Eff '[Hyperbole, IOE] ()) -> Wai.Application
application toDoc actions =
  websocketsOr
    defaultConnectionOptions
    (socketApplication actions)
    (waiApplication toDoc actions)


waiApplication :: (Route route) => (L.ByteString -> L.ByteString) -> (route -> Eff '[Hyperbole, IOE] ()) -> Wai.Application
waiApplication toDoc actions request respond = do
  req <- fromWaiRequest request
  res <- runEff $ runHyperboleRoute req actions
  sendResponse res
 where
  fromWaiRequest wr = do
    bd <- liftIO $ Wai.consumeRequestBodyLazy wr
    pure $ Request (Wai.pathInfo wr) (Wai.queryString wr) bd

  -- TODO: logging?
  sendResponse :: Response -> IO Wai.ResponseReceived
  sendResponse (ErrParse e) = respBadRequest ("Parse Error: " <> cs e)
  sendResponse ErrNoHandler = respBadRequest "No Handler Found"
  sendResponse NotFound = respNotFound
  sendResponse (Response vw) = do
    let body = addDocument (Wai.requestMethod request) (renderLazyByteString vw)
    respHtml body

  respBadRequest e =
    respond $ Wai.responseLBS status400 [contentType ContentText] e

  respNotFound =
    respond $ Wai.responseLBS status404 [contentType ContentText] "Not Found"

  respHtml body = do
    let headers = [contentType ContentHtml]
    respond $ Wai.responseLBS status200 headers body

  -- convert to document if GET. Subsequent POST requests will only include fragments
  addDocument :: Method -> L.ByteString -> L.ByteString
  addDocument "GET" bd = toDoc bd
  addDocument _ bd = bd


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")


socketApplication :: (Route route) => (route -> Eff '[Hyperbole, IOE] ()) -> PendingConnection -> IO ()
socketApplication actions pending = do
  conn <- WS.acceptRequest pending
  forever $ talk conn
 where
  talk :: Connection -> IO ()
  talk conn = do
    res <- runSocket $ do
      req <- request
      liftIO $ print (req.path, req.query, req.body)
      liftIO $ runEff $ runHyperboleRoute req actions

    case res of
      Right (Response vw) -> sendView vw
      Right (ErrParse t) -> sendError $ "ErrParse " <> t
      Right ErrNoHandler -> sendError @Text "ErrNoHandler"
      Right NotFound -> sendError @Text "NotFound"
      Left err -> sendError err
   where
    runSocket :: Eff '[Error SocketError, Reader Connection, IOE] Response -> IO (Either SocketError Response)
    runSocket = runEff . runReader conn . runErrorNoCallStack @SocketError

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
      pure $ Request path query (cs body)

    messageParts :: Text -> Either SocketError ([Text], Query, Text)
    messageParts t = do
      case T.splitOn "\n" t of
        [url, q, body] -> pure (paths url, query q, body)
        [url, q] -> pure (paths url, query q, "")
        _ -> Left $ InvalidMessage t
     where
      paths p = filter (/= "") $ T.splitOn "/" p
      query q = parseQuery (cs q)

    sendView :: View () () -> IO ()
    sendView vw = WS.sendTextData conn $ renderLazyByteString vw

    sendError :: (Show e) => e -> IO ()
    sendError e = WS.sendTextData conn $ pack (show e)


data SocketError
  = InvalidMessage Text
  deriving (Show, Eq)
