{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Application
  ( waiApp
  , websocketsOr
  , defaultConnectionOptions
  , liveApp
  , socketApp
  , runServerSockets
  , runServerWai
  ) where

import Control.Monad (forever)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Network.HTTP.Types (HeaderName, Method, Query, parseQuery, status200, status400, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets (Connection, PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Cookie (parseCookies)
import Web.Hyperbole.Effect
import Web.Hyperbole.Session
import Web.View (View, renderLazyByteString)


liveApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Server, IOE] Response -> Eff '[Hyperbole, Server, Reader Connection, IOE] Response -> Wai.Application
liveApp toDoc wai sock =
  websocketsOr
    defaultConnectionOptions
    (socketApp sock)
    (waiApp toDoc wai)


waiApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Server, IOE] Response -> Wai.Application
waiApp toDoc actions req res = do
  rr <- runEff $ runServerWai toDoc req res $ runHyperbole actions
  case rr of
    Nothing -> error "Missing required response in handler"
    Just r -> pure r


runServerWai
  :: (IOE :> es)
  => (BL.ByteString -> BL.ByteString)
  -> Wai.Request
  -> (Wai.Response -> IO ResponseReceived)
  -> Eff (Server : es) a
  -> Eff es (Maybe Wai.ResponseReceived)
runServerWai toDoc req respond =
  reinterpret runLocal $ \_ -> \case
    LoadRequest -> fromWaiRequest req
    SendResponse sess r -> do
      rr <- liftIO $ sendResponse sess r
      put (Just rr)
 where
  runLocal :: (IOE :> es) => Eff (State (Maybe ResponseReceived) : es) a -> Eff es (Maybe ResponseReceived)
  runLocal = execState Nothing

  sendResponse :: Session -> Response -> IO Wai.ResponseReceived
  sendResponse sess r =
    respond $ response r
   where
    response :: Response -> Wai.Response
    response NotFound = respError status404 "Not Found"
    response Empty = respError status500 "Empty Response"
    response (Err (ErrParse e)) = respError status400 ("Parse Error: " <> cs e)
    response (Err (ErrParam e)) = respError status400 $ "ErrParam: " <> cs e
    response (Err (ErrOther e)) = respError status500 $ "Server Error: " <> cs e
    response (Response vw) =
      respHtml $
        addDocument (Wai.requestMethod req) (renderLazyByteString vw)

    respError s = Wai.responseLBS s [contentType ContentText]

    respHtml body =
      let headers = [contentType ContentHtml, setSessionCookie sess]
       in Wai.responseLBS status200 headers body

  -- convert to document if full page request. Subsequent POST requests will only include fragments
  addDocument :: Method -> BL.ByteString -> BL.ByteString
  addDocument "GET" bd = toDoc bd
  addDocument _ bd = bd

  fromWaiRequest :: (MonadIO m) => Wai.Request -> m Request
  fromWaiRequest wr = do
    body <- liftIO $ Wai.consumeRequestBodyLazy wr
    let isFullPage = Wai.requestMethod wr == "GET"
        path = Wai.pathInfo wr
        query = Wai.queryString wr
        headers = Wai.requestHeaders wr
        cookie = fromMaybe "" $ L.lookup "Cookie" headers
        cookies = parseCookies cookie
    liftIO $ print cookies
    pure $ Request{body, path, query, isFullPage, cookies}


socketApp :: (MonadIO m) => Eff '[Hyperbole, Server, Reader Connection, IOE] Response -> PendingConnection -> m ()
socketApp actions pend = liftIO $ do
  conn <- WS.acceptRequest pend
  forever $ do
    runEff $ runReader conn $ runServerSockets conn $ runHyperbole actions


runServerSockets
  :: (IOE :> es)
  => Connection
  -> Eff (Server : es) Response
  -> Eff es Response
runServerSockets conn = reinterpret runLocal $ \_ -> \case
  LoadRequest -> receiveRequest
  SendResponse _ res -> do
    case res of
      (Response vw) -> sendView vw
      (Err r) -> sendError r
      Empty -> sendError $ ErrOther "Empty"
      NotFound -> sendError $ ErrOther "NotFound"
 where
  runLocal = runErrorNoCallStackWith @SocketError onSocketError

  onSocketError :: (IOE :> es) => SocketError -> Eff es Response
  onSocketError e = do
    let r = ErrOther $ cs $ show e
    sendError r
    pure $ Err r

  sendError :: (IOE :> es) => ResponseError -> Eff es ()
  sendError r = do
    -- conn <- ask @Connection
    -- TODO: better error handling!
    liftIO $ WS.sendTextData conn $ "|ERROR|" <> pack (show r)

  sendView :: (IOE :> es) => View () () -> Eff es ()
  sendView vw = do
    -- conn <- ask @Connection
    liftIO $ WS.sendTextData conn $ renderLazyByteString vw

  receiveRequest :: (IOE :> es, Error SocketError :> es) => Eff es Request
  receiveRequest = do
    t <- receiveText
    case parseMessage t of
      Left e -> throwError e
      Right r -> pure r

  receiveText :: (IOE :> es) => Eff es Text
  receiveText = do
    -- c <- ask @Connection
    liftIO $ WS.receiveData conn

  parseMessage :: Text -> Either SocketError Request
  parseMessage t = do
    (path, query, body) <- messageParts t
    let isFullPage = False
        -- TODO: sessions on sockets
        cookies = []
    pure $ Request{path, query, body = cs body, isFullPage, cookies}

  messageParts :: Text -> Either SocketError ([Text], Query, Text)
  messageParts t = do
    case T.splitOn "\n" t of
      [url, q, body] -> pure (paths url, query q, body)
      [url, q] -> pure (paths url, query q, "")
      _ -> Left $ InvalidMessage t
   where
    paths p = filter (/= "") $ T.splitOn "/" p
    query q = parseQuery (cs q)


data SocketError
  = InvalidMessage Text
  deriving (Show, Eq)


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, BS.ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")
