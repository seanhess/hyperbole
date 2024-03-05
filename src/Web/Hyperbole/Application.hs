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
import Network.HTTP.Types (HeaderName, Method, parseQuery, status200, status302, status400, status401, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets (Connection, PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Cookie (parseCookies)
import Web.Hyperbole.Effect
import Web.Hyperbole.Session
import Web.View (Url (..), View, renderLazyByteString, renderUrl)


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
    LoadRequest -> do
      fromWaiRequest req
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
    response (Err ErrAuth) = respError status401 "Unauthorized"
    response (Response vw) =
      respHtml $
        addDocument (Wai.requestMethod req) (renderLazyByteString vw)
    response (Redirect u) = do
      let headers = ("Location", cs (renderUrl u)) : setCookies
      Wai.responseLBS status302 headers ""

    respError s = Wai.responseLBS s [contentType ContentText]

    respHtml body =
      -- always set the session...
      let headers = contentType ContentHtml : setCookies
       in Wai.responseLBS status200 headers body

    setCookies =
      [("Set-Cookie", sessionSetCookie sess)]

  -- convert to document if full page request. Subsequent POST requests will only include fragments
  addDocument :: Method -> BL.ByteString -> BL.ByteString
  addDocument "GET" bd = toDoc bd
  addDocument _ bd = bd

  fromWaiRequest :: (MonadIO m) => Wai.Request -> m Request
  fromWaiRequest wr = do
    body <- liftIO $ Wai.consumeRequestBodyLazy wr
    let path = Wai.pathInfo wr
        query = Wai.queryString wr
        headers = Wai.requestHeaders wr
        cookie = fromMaybe "" $ L.lookup "Cookie" headers
        host = Host $ fromMaybe "" $ L.lookup "Host" headers
        cookies = parseCookies cookie
        method = Wai.requestMethod wr
    pure $ Request{body, path, query, method, cookies, host}


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
  SendResponse sess res -> do
    case res of
      (Response vw) -> sendView (addMetadata sess) vw
      (Err r) -> sendError r
      Empty -> sendError $ ErrOther "Empty"
      NotFound -> sendError $ ErrOther "NotFound"
      (Redirect url) -> sendRedirect (addMetadata sess) url
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

  sendView :: (IOE :> es) => (BL.ByteString -> BL.ByteString) -> View () () -> Eff es ()
  sendView addMeta vw = do
    -- conn <- ask @Connection
    liftIO $ WS.sendTextData conn $ addMeta $ renderLazyByteString vw

  sendRedirect :: (IOE :> es) => (BL.ByteString -> BL.ByteString) -> Url -> Eff es ()
  sendRedirect addMeta u = do
    -- conn <- ask @Connection
    liftIO $ WS.sendTextData conn $ addMeta $ "|REDIRECT|" <> cs (renderUrl u)

  addMetadata :: Session -> BL.ByteString -> BL.ByteString
  addMetadata sess cont =
    -- you may have 1 or more lines containing metadata followed by a view
    -- \|SESSION| key=value; another=woot;
    -- <div w,kasdlkfjasdfkjalsdf kl>
    sessionLine <> "\n" <> cont
   where
    metaLine name value = "|" <> name <> "|" <> value

    sessionLine :: BL.ByteString
    sessionLine = metaLine "SESSION" $ cs (sessionSetCookie sess)

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
    case T.splitOn "\n" t of
      [url, host, cook, body] -> parse url cook host (Just body)
      [url, host, cook] -> parse url cook host Nothing
      _ -> Left $ InvalidMessage t
   where
    parseUrl :: Text -> Either SocketError (Text, Text)
    parseUrl u =
      case T.splitOn "?" u of
        [url, query] -> pure (url, query)
        _ -> Left $ InvalidMessage u

    parse :: Text -> Text -> Text -> Maybe Text -> Either SocketError Request
    parse url cook hst mbody = do
      (u, q) <- parseUrl url
      let path = paths u
          query = parseQuery (cs q)
          cookies = parseCookies $ cs $ header cook
          host = Host $ cs $ header hst
          method = "POST"
          body = cs $ fromMaybe "" mbody
      pure $ Request{path, host, query, body, method, cookies}

    paths p = filter (/= "") $ T.splitOn "/" p

    -- drop up to the colon, then ': '
    header = T.drop 2 . T.dropWhile (/= ':')


data SocketError
  = InvalidMessage Text
  deriving (Show, Eq)


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, BS.ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")
