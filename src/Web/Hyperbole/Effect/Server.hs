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
import Network.HTTP.Types (HeaderName, Method, Query, status200, status400, status401, status404, status500, urlDecode, urlEncode)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie (parseCookies)
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.Session as Cookies
import Web.Hyperbole.Route
import Web.View (Segment, View, renderLazyByteString, renderUrl)


-- | Low level effect mapping request/response to either HTTP or WebSockets
data Server :: Effect where
  LoadRequest :: Server m Request
  SendResponse :: Client -> Response -> Server m ()


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
    SendResponse client r -> do
      rr <- liftIO $ sendResponse client r
      put (Just rr)
 where
  runLocal :: (IOE :> es) => Eff (State (Maybe ResponseReceived) : es) a -> Eff es (Maybe ResponseReceived)
  runLocal = execState Nothing

  sendResponse :: Client -> Response -> IO Wai.ResponseReceived
  sendResponse client r =
    respond $ response r
   where
    response :: Response -> Wai.Response
    response NotFound = respError status404 "Not Found"
    response Empty = respError status500 "Empty Response"
    response (Err (ErrParse e)) = respError status400 ("Parse Error: " <> cs e)
    response (Err (ErrQuery e)) = respError status400 $ "ErrQuery: " <> cs e
    response (Err (ErrSession param e)) = respError status400 $ "ErrSession: " <> cs (show param) <> " " <> cs e
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
      let headers = contentType ContentHtml : (setCookies <> setQuery client.query)
       in Wai.responseLBS status200 headers body

    setCookies =
      fmap setCookie $ Cookies.toList client.session

    setCookie :: Cookie -> (HeaderName, BS.ByteString)
    setCookie cookie =
      ("Set-Cookie", renderCookie (Wai.pathInfo req) cookie)

    setQuery Nothing = []
    setQuery (Just qd) =
      [("Set-Query", QueryData.render qd)]

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
        requestId = RequestId $ cs $ fromMaybe "" $ L.lookup "Request-Id" headers
        cookies = cookiesFromHeader (parseCookies cookie)
        method = Wai.requestMethod wr
    pure $ Request{body, path, query, method, cookies, host, requestId}


renderCookie :: [Segment] -> Cookie -> BS.ByteString
renderCookie requestPath cookie =
  let path = fromMaybe requestPath cookie.path
   in key <> "=" <> value cookie.value <> "; SameSite=None; secure; path=" <> cs (renderUrl (pathUrl path))
 where
  key = cs cookie.key.text
  value Nothing = "; expires=Thu, 01 Jan 1970 00:00:00 GMT"
  value (Just val) = urlEncode True (cs val.text)


cookiesFromHeader :: [(BS.ByteString, BS.ByteString)] -> Cookies
cookiesFromHeader cks = do
  Cookies.fromList $ fmap toCookie cks
 where
  toCookie (k, v) =
    let value = ParamValue (cs $ urlDecode True v)
        key = Param (cs k)
     in Cookie key (Just value) Nothing


runServerSockets
  :: (IOE :> es)
  => Connection
  -> Request
  -> Eff (Server : es) Response
  -> Eff es Response
runServerSockets conn req = reinterpret runLocal $ \_ -> \case
  LoadRequest -> pure req -- receiveRequest conn
  SendResponse client res -> do
    case res of
      (Response vid vw) -> do
        sendView client vid vw
      (Err r) -> sendError r
      Empty -> sendError $ ErrOther "Empty"
      NotFound -> sendError $ ErrOther "NotFound"
      (Redirect url) -> sendRedirect client url
 where
  runLocal = runErrorNoCallStackWith @SocketError onSocketError

  onSocketError :: (IOE :> es) => SocketError -> Eff es Response
  onSocketError e = do
    let r = ErrOther $ cs $ show e
    sendError r
    pure $ Err r

  -- low level message. Use sendResponse
  sendMessage :: (MonadIO m) => Metadata -> BL.ByteString -> m ()
  sendMessage meta' cnt = do
    let msg = renderMetadata meta' <> "\n" <> cnt
    liftIO $ WS.sendTextData conn msg

  -- send response with client metadata
  sendResponse :: (MonadIO m) => Client -> Metadata -> BL.ByteString -> m ()
  sendResponse client meta =
    sendMessage (responseMeta <> meta)
   where
    responseMeta :: Metadata
    responseMeta =
      metaRequestId client.requestId <> metaSession client.session <> metaQuery client.query

  sendError :: (IOE :> es) => ResponseError -> Eff es ()
  sendError r = do
    -- TODO: better error handling!
    sendMessage (metaRequestId req.requestId <> metadata "ERROR" (pack (show r))) ""

  sendView :: (IOE :> es) => Client -> TargetViewId -> View () () -> Eff es ()
  sendView client vid vw = do
    sendResponse client (viewIdMeta vid) (renderLazyByteString vw)

  renderMetadata :: Metadata -> BL.ByteString
  renderMetadata (Metadata m) = BL.intercalate "\n" $ fmap (uncurry metaLine) m

  sendRedirect :: (IOE :> es) => Client -> Url -> Eff es ()
  sendRedirect client u = do
    sendResponse client (metadata "REDIRECT" (renderUrl u)) ""

  metaSession :: Cookies -> Metadata
  metaSession cookies = mconcat $ fmap metaCookie $ Cookies.toList cookies
   where
    metaCookie :: Cookie -> Metadata
    metaCookie cookie =
      Metadata [("COOKIE", cs (renderCookie req.path cookie))]

  metaRequestId :: RequestId -> Metadata
  metaRequestId (RequestId reqId) =
    Metadata [("REQUEST-ID", cs reqId)]

  metaQuery :: Maybe QueryData -> Metadata
  metaQuery Nothing = mempty
  metaQuery (Just q) =
    Metadata [("QUERY", cs $ QueryData.render q)]

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


data Client = Client
  { requestId :: RequestId
  , session :: Cookies
  , query :: Maybe QueryData
  }


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


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


data Request = Request
  { host :: Host
  , path :: [Segment]
  , query :: Query
  , body :: BL.ByteString
  , method :: Method
  , cookies :: Cookies
  , requestId :: RequestId
  }
  deriving (Show)


newtype RequestId = RequestId Text
  deriving (Show)


-- | Valid responses for a 'Hyperbole' effect. Use 'notFound', etc instead.
data Response
  = Response TargetViewId (View () ())
  | NotFound
  | Redirect Url
  | Err ResponseError
  | Empty


data ResponseError
  = ErrParse Text
  | ErrQuery Text
  | ErrSession Param Text
  | ErrOther Text
  | ErrNotHandled (Event Text Text)
  | ErrAuth
  deriving (Show)


-- | Serialized ViewId
newtype TargetViewId = TargetViewId Text


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action
