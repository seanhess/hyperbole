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
import Network.HTTP.Types (Header, HeaderName, Method, status200, status400, status401, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie (parseCookies)
import Web.Hyperbole.Route
import Web.Hyperbole.Session
import Web.View (Query, Segment, View, renderLazyByteString, renderUrl)


-- | Low level effect mapping request/response to either HTTP or WebSockets
data Server :: Effect where
  LoadRequest :: Server m Request
  SendResponse :: ResponseMeta -> Response -> Server m ()


type instance DispatchOf Server = 'Dynamic


data ResponseMeta = ResponseMeta
  { session :: Session
  , triggers :: [Trigger]
  }
  deriving (Show)


data Trigger = Trigger TargetViewId TargetAction
  deriving (Show)


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
    SendResponse (ResponseMeta sess triggs) r -> do
      rr <- liftIO $ sendResponse sess triggs r
      put (Just rr)
 where
  runLocal :: (IOE :> es) => Eff (State (Maybe ResponseReceived) : es) a -> Eff es (Maybe ResponseReceived)
  runLocal = execState Nothing

  sendResponse :: Session -> [Trigger] -> Response -> IO Wai.ResponseReceived
  sendResponse sess triggs r =
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
      let headers = ("Location", cs url) : standardHeaders
      Wai.responseLBS status200 headers $ "<script>window.location = '" <> cs url <> "'</script>"

    respError s = Wai.responseLBS s [contentType ContentText]

    respHtml body =
      let headers = standardHeaders <> addTriggers triggs
       in Wai.responseLBS status200 headers body

    standardHeaders = contentType ContentHtml : [setCookies]

    setCookies :: Header
    setCookies =
      ("Set-Cookie", sessionSetCookie sess)

    addTriggers :: [Trigger] -> [Header]
    addTriggers ts =
      [("HYP-TRIGGER", BS.intercalate "||" $ fmap triggerLine ts)]

    triggerLine t =
      let Metadata _ val = metaTrigger t
       in cs val

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
  LoadRequest -> pure req
  SendResponse rmeta res -> do
    case res of
      (Response vid vw) -> sendView rmeta vid vw
      (Err r) -> sendError r
      Empty -> sendError $ ErrOther "Empty"
      NotFound -> sendError $ ErrOther "NotFound"
      (Redirect url) -> sendRedirect rmeta url
 where
  runLocal = runErrorNoCallStackWith @SocketError onSocketError

  onSocketError :: (IOE :> es) => SocketError -> Eff es Response
  onSocketError e = do
    let r = ErrOther $ cs $ show e
    sendError r
    pure $ Err r

  sendError :: (IOE :> es) => ResponseError -> Eff es ()
  sendError r = do
    -- TODO: better error handling!
    sendMessage conn [Metadata "ERROR" (pack (show r))] ""

  sendView :: (IOE :> es) => ResponseMeta -> TargetViewId -> View () () -> Eff es ()
  sendView rmeta vid vw = do
    let meta = metaViewId vid : metaSession rmeta.session : fmap metaTrigger rmeta.triggers
    sendMessage conn meta (renderLazyByteString vw)

  sendRedirect :: (IOE :> es) => ResponseMeta -> Url -> Eff es ()
  sendRedirect metas u = do
    sendMessage conn [metaRedirect u, metaSession metas.session] ""

  metaSession :: Session -> Metadata
  metaSession sess = Metadata "SESSION" (cs $ sessionSetCookie sess)

  metaViewId :: TargetViewId -> Metadata
  metaViewId (TargetViewId vid) = Metadata "VIEW-ID" vid

  metaRedirect :: Url -> Metadata
  metaRedirect u = Metadata "REDIRECT" (renderUrl u)


sendMessage :: (MonadIO m) => Connection -> [Metadata] -> BL.ByteString -> m ()
sendMessage conn metas cnt = do
  let msg = renderMetadata metas <> "\n" <> cnt
  liftIO $ WS.sendTextData conn msg
 where
  renderMetadata :: [Metadata] -> BL.ByteString
  renderMetadata = BL.intercalate "\n" . fmap renderMetaLine

  renderMetaLine :: Metadata -> BL.ByteString
  renderMetaLine (Metadata name value) = "|" <> cs name <> "|" <> cs value


metaTrigger :: Trigger -> Metadata
metaTrigger (Trigger (TargetViewId viewId) (TargetAction act)) =
  Metadata "TRIGGER" (viewId <> "|" <> act)


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


data Metadata = Metadata {key :: Text, value :: Text}


newtype Host = Host {text :: BS.ByteString}
  deriving (Show)


data Request = Request
  { host :: Host
  , path :: [Segment]
  , query :: Query
  , body :: BL.ByteString
  , method :: Method
  , cookies :: [(BS.ByteString, BS.ByteString)]
  }
  deriving (Show)


{- | Valid responses for a 'Hyperbole' effect. Use 'notFound', etc instead. Reminds you to use 'load' in your 'Page'

> myPage :: (Hyperbole :> es) => Page es Response
> myPage = do
>   -- compiler error: () does not equal Response
>   pure ()
-}
data Response
  = Response TargetViewId (View () ())
  | NotFound
  | Redirect Url
  | Err ResponseError
  | Empty


data ResponseError
  = ErrParse Text
  | ErrParam Text
  | ErrOther Text
  | ErrNotHandled (Event Text Text)
  | ErrAuth
  deriving (Show)


-- | Serialized ViewId
newtype TargetViewId = TargetViewId Text
  deriving (Show)


-- | Serialized ViewAction
newtype TargetAction = TargetAction Text
  deriving (Show)


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action
