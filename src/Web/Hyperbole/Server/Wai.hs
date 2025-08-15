{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Server.Wai where

import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Effectful
import Effectful.Exception (throwIO)
import Network.HTTP.Types (Header, HeaderName, status200, status400, status401, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Web.Atomic (att, (@))
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.Encoded (Encoded, encodedParseText, encodedToText)
import Web.Hyperbole.Data.URI (path, uriToText)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Server.Message
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View (renderLazyByteString)
import Web.Hyperbole.View.Tag
import Web.Hyperbole.View.Types (View)


handleRequestWai
  :: (IOE :> es)
  => (BL.ByteString -> BL.ByteString)
  -> Wai.Request
  -> (Wai.Response -> IO ResponseReceived)
  -> Eff (Hyperbole : es) Response
  -> Eff es Wai.ResponseReceived
handleRequestWai toDoc req respond actions = do
  -- NOTE: This is called for both updates AND for page loads
  body <- liftIO $ Wai.consumeRequestBodyLazy req
  rq <- either throwIO pure $ do
    fromWaiRequest req body
  (res, client, rmts) <- runHyperbole rq actions
  liftIO $ sendResponse addDocument rq respond client res rmts
 where
  -- convert to document if full page request. Subsequent POST requests will only include html fragments for updates
  addDocument :: BL.ByteString -> BL.ByteString
  addDocument bd =
    case Wai.requestMethod req of
      "GET" -> toDoc $ "\n" <> bd
      _ -> bd


sendResponse :: (BL.ByteString -> BL.ByteString) -> Request -> (Wai.Response -> IO ResponseReceived) -> Client -> Response -> [Remote] -> IO Wai.ResponseReceived
sendResponse addDoc req respond client res remotes = do
  let meta = requestMetadata req <> responseMetadata req.path client remotes
  respond $ response meta res
 where
  response :: Metadata -> Response -> Wai.Response
  response meta = \case
    NotFound -> respondText status404 [] "Not Found"
    (Err err) ->
      case err of
        ErrParse e -> respondText status400 [] ("Parse Error: " <> cs e)
        ErrQuery e -> respondText status400 [] $ "ErrQuery: " <> cs e
        ErrSession param e -> respondText status400 [] $ "ErrSession: " <> cs (show param) <> " " <> cs e
        ErrServer msg -> do
          respondText status500 [] $ "Server Error: " <> cs msg
        ErrCustom _ body -> do
          let out = addDoc (renderLazyByteString body)
          respondHtml status500 [] out
        ErrInternal -> respondText status500 [] "Internal Server Error"
        ErrAuth m -> respondText status401 [] $ "Unauthorized: " <> cs m
        ErrNotHandled e -> respondText status400 [] $ cs $ errNotHandled e
    (Response _ vw) -> do
      respondHtml status200 (clientHeaders client) $ renderViewResponse meta vw
    (Redirect u) -> do
      let url = uriToText u
      -- We have to use a 200 javascript redirect because javascript
      -- will redirect the fetch(), while we want to redirect the whole page
      -- see index.ts sendAction()
      let hs = ("Location", cs url) : clientHeaders client
      respondHtml status200 hs $ renderViewResponse (metaRedirect u <> meta) $ do
        el "Redirecting"
        script'
          -- static script is safe to execute
          [i|
            let metaInput = document.getElementById("hyp.metadata").innerText;
            let meta = Hyperbole.parseMetadata(metaInput)
            if (meta.redirect) {
              window.location = meta.redirect
            }
            else {
              console.error("Invalid Redirect", meta.rediect)
            }
          |]

  renderViewResponse :: Metadata -> View () () -> BL.ByteString
  renderViewResponse meta vw =
    addDoc $
      scriptMeta meta <> "\n\n" <> renderLazyByteString vw

  respondText s hs = Wai.responseLBS s (contentType ContentText : hs)
  respondHtml s hs = Wai.responseLBS s (contentType ContentHtml : hs)

  -- via HTTP, we want to manually set some headers rather than just rely on the client
  clientHeaders :: Client -> [Header]
  clientHeaders = setCookies
   where
    setCookies clnt =
      fmap setCookie $ Cookie.toList clnt.session

    setCookie :: Cookie -> (HeaderName, BS.ByteString)
    setCookie cookie =
      ("Set-Cookie", Cookie.render req.path cookie)


scriptMeta :: Metadata -> BL.ByteString
scriptMeta m =
  renderLazyByteString $
    script' @ type_ "application/hyp.metadata" . att "id" "hyp.metadata" $
      cs $
        "\n" <> renderMetadata m <> "\n"


messageFromBody :: BL.ByteString -> Either MessageError Message
messageFromBody inp = do
  first (\e -> InvalidMessage e (cs inp)) $ parseActionMessage (cs inp)


fromWaiRequest :: Wai.Request -> BL.ByteString -> Either MessageError Request
fromWaiRequest wr body = do
  let pth = path $ cs $ Wai.rawPathInfo wr
      query = Wai.queryString wr
      headers = Wai.requestHeaders wr
      cookie = fromMaybe "" $ L.lookup "Cookie" headers
      host = Host $ fromMaybe "" $ L.lookup "Host" headers
      requestId = RequestId $ cs $ fromMaybe "" $ L.lookup "Hyp-RequestId" headers
      method = Wai.requestMethod wr
      event = lookupEvent headers

  cookies <- fromCookieHeader cookie

  pure $
    Request
      { body = body
      , event
      , path = pth
      , query = queryRemoveSystem query
      , method
      , cookies
      , host
      , requestId
      }
 where
  lookupEvent :: [Header] -> Maybe (Event TargetViewId Encoded)
  lookupEvent headers = do
    viewId <- TargetViewId . cs <$> L.lookup "Hyp-ViewId" headers
    actText <- cs <$> L.lookup "Hyp-Action" headers
    case encodedParseText actText of
      Left _ -> Nothing
      Right a -> pure $ Event viewId a


-- Client only returns ONE Cookie header, with everything concatenated
fromCookieHeader :: BS.ByteString -> Either MessageError Cookies
fromCookieHeader h =
  case Cookie.parse (Web.Cookie.parseCookies h) of
    Left err -> Left $ InvalidCookie h err
    Right a -> pure a


errNotHandled :: Event TargetViewId Encoded -> String
errNotHandled ev =
  L.intercalate
    "\n"
    [ "No Handler for Event viewId: " <> cs ev.viewId.text <> " action: " <> cs (encodedToText ev.action)
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
