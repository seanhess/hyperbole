{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Server.Wai where

import Control.Exception (throwIO)
import Data.Aeson (ToJSON, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effectful
import Network.HTTP.Types (Header, HeaderName, Method, status200, status400, status401, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (path, uriToText)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Server.Types
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View (renderLazyByteString)


handleRequestWai
  :: (IOE :> es)
  => (BL.ByteString -> BL.ByteString)
  -> Wai.Request
  -> (Wai.Response -> IO ResponseReceived)
  -> Eff (Hyperbole : es) Response
  -> Eff es Wai.ResponseReceived
handleRequestWai toDoc req respond actions = do
  rq <- fromWaiRequest req
  (res, client, rmts) <- runHyperbole rq actions
  liftIO $ sendResponse toDoc req respond client res rmts


sendResponse :: (BL.ByteString -> BL.ByteString) -> Wai.Request -> (Wai.Response -> IO ResponseReceived) -> Client -> Response -> [Remote] -> IO Wai.ResponseReceived
sendResponse toDoc req respond client res remotes = do
  respond $ response res
 where
  response :: Response -> Wai.Response
  response = \case
    NotFound -> respError status404 "Not Found"
    (Err err) ->
      case err of
        ErrParse e -> respError status400 ("Parse Error: " <> cs e)
        ErrQuery e -> respError status400 $ "ErrQuery: " <> cs e
        ErrSession param e -> respError status400 $ "ErrSession: " <> cs (show param) <> " " <> cs e
        ErrServer msg -> do
          respError status500 $ "Server Error: " <> cs msg
        ErrCustom _ body -> do
          let out = addDocument (renderLazyByteString body)
          Wai.responseLBS status500 [contentType ContentHtml] out
        ErrInternal -> respError status500 "Internal Server Error"
        ErrAuth m -> respError status401 $ "Unauthorized: " <> cs m
        ErrNotHandled e -> respError status400 $ cs $ errNotHandled e
    (Response _ vw) ->
      let hs = contentType ContentHtml : metaHeaders
       in Wai.responseLBS status200 hs $ addDocument $ addRemotes $ renderLazyByteString vw
    (Redirect u) -> do
      let url = uriToText u
      -- We have to use a 200 javascript redirect because javascript
      -- will redirect the fetch(), while we want to redirect the whole page
      -- see index.ts sendAction()
      let hs = ("Location", cs url) : contentType ContentHtml : metaHeaders
      Wai.responseLBS status200 hs $ addDocument $ addRemotes [i|<script>window.location = '#{url}'</script>|]

  respError s = Wai.responseLBS s [contentType ContentText]

  metaHeaders :: [Header]
  metaHeaders = setQuery client.query <> (setRequestId client.requestId : setCookies)

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
  addDocument :: BL.ByteString -> BL.ByteString
  addDocument bd =
    case Wai.requestMethod req of
      "GET" -> toDoc bd
      _ -> bd

  -- TODO: the problem is that we've simply rendered the html for the page...
  -- http is different. For sockets, we could send the message out-of-band?
  -- I'm still annoyed by the WAI fallback
  -- it's overfly complicated
  addRemotes :: BL.ByteString -> BL.ByteString
  addRemotes body =
    let events = filter isEvent remotes
        actions = filter isAction remotes
     in BLC.intercalate "\n" [body, scriptData "events" events, scriptData "actions" actions]
   where
    isEvent RemoteEvent{} = True
    isEvent _ = False

    isAction RemoteAction{} = True
    isAction _ = False

  -- TODO: escape </script>
  scriptData :: (ToJSON a) => Text -> a -> BL.ByteString
  scriptData sid a =
    [i|<script id="#{sid}" type="application/json">#{encode a}</script>|]


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


-- Client only returns ONE Cookie header, with everything concatenated
fromCookieHeader :: (MonadIO m) => BS.ByteString -> m Cookies
fromCookieHeader h =
  case Cookie.parse (Web.Cookie.parseCookies h) of
    Left err -> liftIO $ throwIO $ InvalidCookie h err
    Right a -> pure a


errNotHandled :: Event TargetViewId Text -> String
errNotHandled ev =
  L.intercalate
    "\n"
    [ "No Handler for Event viewId: " <> cs ev.viewId.text <> " action: " <> cs ev.action
    , "<p>Remember to add a `hyper` handler in your page function</p>"
    , "<pre>"
    , "page :: (Hyperbole :> es) => Page es Response"
    , "page = do"
    , "  handle contentsHandler"
    , "  load $ do"
    , "    pure $ hyper Contents contentsView"
    , "</pre>"
    ]


contentType :: ContentType -> (HeaderName, BS.ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")
