{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Application
  ( waiApp
  , websocketsOr
  , defaultConnectionOptions
  , liveApp
  , socketApp
  , runServerSockets
  , runServerWai
  , basicDocument
  , routeRequest
  ) where

import Control.Monad (forever)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types (HeaderName, Method, parseQuery, status200, status400, status401, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Internal (ResponseReceived (..))
import Network.WebSockets (Connection, PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Cookie (parseCookies)
import Web.Hyperbole.Effect
import Web.Hyperbole.Embed (cssResetEmbed, scriptEmbed)
import Web.Hyperbole.Route
import Web.Hyperbole.Session
import Web.View (View, renderLazyByteString, renderUrl)


{- | Turn one or more 'Page's into a Wai Application. Respond using both HTTP and WebSockets

> main = do
>   run 3000 $ do
>   liveApp (basicDocument "Example") $ do
>      page mainPage
-}
liveApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Server, IOE] Response -> Wai.Application
liveApp toDoc app =
  websocketsOr
    defaultConnectionOptions
    (runEff . socketApp app)
    (waiApp toDoc app)


socketApp :: (IOE :> es) => Eff (Hyperbole : Server : es) Response -> PendingConnection -> Eff es ()
socketApp actions pend = do
  conn <- liftIO $ WS.acceptRequest pend
  forever $ do
    runServerSockets conn $ runHyperbole actions


waiApp :: (BL.ByteString -> BL.ByteString) -> Eff '[Hyperbole, Server, IOE] Response -> Wai.Application
waiApp toDoc actions req res = do
  rr <- runEff $ runServerWai toDoc req res $ runHyperbole actions
  case rr of
    Nothing -> error "Missing required response in handler"
    Just r -> pure r


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
  -> Eff (Server : es) Response
  -> Eff es Response
runServerSockets conn = reinterpret runLocal $ \_ -> \case
  LoadRequest -> receiveRequest
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


newtype Metadata = Metadata [(BL.ByteString, Text)]
  deriving newtype (Semigroup, Monoid)


data SocketError
  = InvalidMessage Text
  deriving (Show, Eq)


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, BS.ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")


{- | wrap HTML fragments in a simple document with a custom title and include required embeds

@
'liveApp' (basicDocument "App Title") ('routeRequest' router)
@

You may want to specify a custom document function instead:

> myDocument :: ByteString -> ByteString
> myDocument content =
>   [i|<html>
>     <head>
>       <title>#{title}</title>
>       <script type="text/javascript">#{scriptEmbed}</script>
>       <style type type="text/css">#{cssResetEmbed}</style>
>     </head>
>     <body>#{content}</body>
>   </html>|]
-}
basicDocument :: Text -> BL.ByteString -> BL.ByteString
basicDocument title cnt =
  [i|<html>
      <head>
        <title>#{title}</title>
        <script type="text/javascript">#{scriptEmbed}</script>
        <style type type="text/css">#{cssResetEmbed}</style>
      </head>
      <body>#{cnt}</body>
  </html>|]


{- | Route URL patterns to different pages


@
import Page.Messages qualified as Messages
import Page.Users qualified as Users

data AppRoute
  = Main
  | Messages
  | Users UserId
  deriving (Eq, Generic, 'Route')

router :: ('Hyperbole' :> es) => AppRoute -> 'Eff' es 'Response'
router Messages = 'page' Messages.page
router (Users uid) = 'page' $ Users.page uid
router Main = do
  'view' $ do
    'el_' "click a link below to visit a page"
    'route' Messages id \"Messages\"

main = do
  'run' 3000 $ do
    'liveApp' ('basicDocument' \"Example\") (routeRequest router)
@
-}
routeRequest :: (Hyperbole :> es, Route route) => (route -> Eff es Response) -> Eff es Response
routeRequest actions = do
  path <- reqPath
  case findRoute path of
    Nothing -> send $ RespondEarly NotFound
    Just rt -> actions rt
