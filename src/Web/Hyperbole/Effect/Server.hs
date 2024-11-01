{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.Server where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types (HeaderName, Method, status200, status400, status401, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie (parseCookies)
import Web.Hyperbole.Effect.Request
import Web.Hyperbole.Route
import Web.Hyperbole.Session
import Web.View (View, renderLazyByteString, renderUrl)


-- | Low level effect mapping request/response to either HTTP or WebSockets
data Server :: Effect where
  LoadRequest :: Server m Request
  SendResponse :: Session -> Response -> Server m ()


type instance DispatchOf Server = 'Dynamic


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
    response (Err (ErrNotHandled e)) = respError status400 $ cs $ errNotHandled e
    response (Response _ vw) =
      respHtml $
        addDocument (Wai.requestMethod req) (renderLazyByteString vw)
    response (Redirect u) = do
      let url = renderUrl u
      -- We have to use a 200 javascript redirect because javascript
      -- will redirect the fetch(), while we want to redirect the whole page
      -- see index.ts sendAction()
      let headers = ("Location", cs url) : contentType ContentHtml : setCookies
      Wai.responseLBS status200 headers $ "<script>window.location = '" <> cs url <> "'</script>"

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


runServerSockets
  :: (IOE :> es)
  => Connection
  -> Request
  -> Eff (Server : es) Response
  -> Eff es Response
runServerSockets conn req = reinterpret runLocal $ \_ -> \case
  LoadRequest -> pure req -- receiveRequest conn
  SendResponse sess res -> do
    case res of
      (Response vid vw) -> sendView (sessionMeta sess) vid vw
      (Err r) -> sendError r
      Empty -> sendError $ ErrOther "Empty"
      NotFound -> sendError $ ErrOther "NotFound"
      (Redirect url) -> sendRedirect (sessionMeta sess) url
 where
  runLocal = runErrorNoCallStackWith @SocketError onSocketError

  onSocketError :: (IOE :> es) => SocketError -> Eff es Response
  onSocketError e = do
    let r = ErrOther $ cs $ show e
    sendError r
    pure $ Err r

  sendMessage :: (MonadIO m) => Metadata -> BL.ByteString -> m ()
  sendMessage meta cnt = do
    let msg = renderMetadata meta <> "\n" <> cnt
    liftIO $ WS.sendTextData conn msg

  sendError :: (IOE :> es) => ResponseError -> Eff es ()
  sendError r = do
    -- TODO: better error handling!
    sendMessage (metadata "ERROR" (pack (show r))) ""

  sendView :: (IOE :> es) => Metadata -> TargetViewId -> View () () -> Eff es ()
  sendView meta vid vw = do
    sendMessage (viewIdMeta vid <> meta) (renderLazyByteString vw)

  renderMetadata :: Metadata -> BL.ByteString
  renderMetadata (Metadata m) = BL.intercalate "\n" $ fmap (uncurry metaLine) m

  sendRedirect :: (IOE :> es) => Metadata -> Url -> Eff es ()
  sendRedirect meta u = do
    -- conn <- ask @Connection
    let r = metadata "REDIRECT" (renderUrl u)
    sendMessage (r <> meta) ""

  sessionMeta :: Session -> Metadata
  sessionMeta sess = Metadata [("SESSION", cs (sessionSetCookie sess))]

  viewIdMeta :: TargetViewId -> Metadata
  viewIdMeta (TargetViewId vid) = Metadata [("VIEW-ID", cs vid)]

  metadata :: BL.ByteString -> Text -> Metadata
  metadata name value = Metadata [(name, value)]

  metaLine :: BL.ByteString -> Text -> BL.ByteString
  metaLine name value = "|" <> name <> "|" <> cs value


errNotHandled :: Event Text Text -> String
errNotHandled ev =
  L.intercalate
    "\n"
    [ "No Handler for Event viewId: " <> cs ev.viewId <> " action: " <> cs ev.action
    , "<p>Remember to add a `hyper` handler in your page function</p>"
    , "<pre>"
    , "page :: (Hyperbole :> es) => Page es Response"
    , "page = do"
    , "  handle contentsHandler"
    , "  load $ do"
    , "    pure $ hyper Contents contentsView"
    , "</pre>"
    ]


data SocketError
  = InvalidMessage Text
  deriving (Show, Eq)


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, BS.ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")


newtype Metadata = Metadata [(BL.ByteString, Text)]
  deriving newtype (Semigroup, Monoid)
