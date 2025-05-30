{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.Server where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
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
import Network.HTTP.Types (Header, HeaderName, Method, Query, QueryItem, status200, status400, status401, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (Path, URI, path, uriToText)
import Web.Hyperbole.View (View, renderLazyByteString)


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
    response (Err (ErrBadEvent ev e)) = respError status500 $ "Bad Event: " <> cs (show ev) <> " " <> cs e
    response (Err ErrAuth) = respError status401 "Unauthorized"
    response (Err (ErrNotHandled e)) = respError status400 $ cs $ errNotHandled e
    response (Response _ vw) =
      respHtml $
        addDocument (Wai.requestMethod req) (renderLazyByteString vw)
    response (Redirect u) = do
      let url = uriToText u
      -- We have to use a 200 javascript redirect because javascript
      -- will redirect the fetch(), while we want to redirect the whole page
      -- see index.ts sendAction()
      let hs = ("Location", cs url) : contentType ContentHtml : headers
      Wai.responseLBS status200 hs $ "<script>window.location = '" <> cs url <> "'</script>"

    respError s = Wai.responseLBS s [contentType ContentText]

    respHtml body =
      -- always set the session...
      let hs = contentType ContentHtml : (setQuery client.query <> headers)
       in Wai.responseLBS status200 hs body

    headers :: [Header]
    headers = setRequestId client.requestId : setCookies

    setCookies =
      fmap setCookie $ Cookie.toList client.session

    setCookie :: Cookie -> (HeaderName, BS.ByteString)
    setCookie cookie =
      ("Set-Cookie", Cookie.render (path $ cs $ Wai.rawPathInfo req) cookie)

    setRequestId :: RequestId -> (HeaderName, BS.ByteString)
    setRequestId (RequestId rid) =
      ("Request-Id", cs rid)

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
    let pth = path $ cs $ Wai.rawPathInfo wr
        query = Wai.queryString wr
        headers = Wai.requestHeaders wr
        cookie = fromMaybe "" $ L.lookup "Cookie" headers
        host = Host $ fromMaybe "" $ L.lookup "Host" headers
        requestId = RequestId $ cs $ fromMaybe "" $ L.lookup "Request-Id" headers
        method = Wai.requestMethod wr

    cookies <- fromCookieHeader cookie

    pure $ Request{body, path = pth, query, method, cookies, host, requestId}


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

  sendRedirect :: (IOE :> es) => Client -> URI -> Eff es ()
  sendRedirect client u = do
    sendResponse client (metadata "REDIRECT" (uriToText u)) ""

  metaSession :: Cookies -> Metadata
  metaSession cookies = mconcat $ fmap metaCookie $ Cookie.toList cookies
   where
    metaCookie :: Cookie -> Metadata
    metaCookie cookie =
      Metadata [("COOKIE", cs (Cookie.render req.path cookie))]

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


-- Client only returns ONE Cookie header, with everything concatenated
fromCookieHeader :: (MonadIO m) => BS.ByteString -> m Cookies
fromCookieHeader h =
  case Cookie.parse (Web.Cookie.parseCookies h) of
    Left err -> liftIO $ throwIO $ InvalidCookie h err
    Right a -> pure a


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


data InternalServerError
  = InvalidCookie BS.ByteString Text
  deriving (Show, Exception)


data SocketError
  = InvalidMessage Text
  | InternalSocket InternalServerError
  deriving (Show)


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
  , path :: Path
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
  | Redirect URI
  | Err ResponseError
  | Empty


data ResponseError
  = ErrParse Text
  | ErrBadEvent (Event Text Text) Text
  | ErrQuery Text
  | ErrSession Text Text
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


lookupEvent :: Query -> Maybe (Event Text Text)
lookupEvent q = do
  viewId <- lookupParam "hyp-id" q
  action <- lookupParam "hyp-action" q
  pure $ Event{viewId, action}


-- | Lower-level lookup straight from the request
lookupParam :: ByteString -> Query -> Maybe Text
lookupParam key q = do
  mval <- L.lookup key q
  val <- mval
  pure $ cs val


isSystemParam :: QueryItem -> Bool
isSystemParam ("hyp-id", _) = True
isSystemParam ("hyp-action", _) = True
isSystemParam _ = False
