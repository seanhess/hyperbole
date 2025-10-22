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
import Web.Hyperbole.Data.URI (path, uriToText)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Server.Message
import Web.Hyperbole.Server.Options
import Web.Hyperbole.Types.Client
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View (View, addContext, renderLazyByteString, script', type_)


handleRequestWai
  :: (IOE :> es)
  => ServerOptions
  -> Wai.Request
  -> (Wai.Response -> IO ResponseReceived)
  -> Eff (Hyperbole : es) Response
  -> Eff es Wai.ResponseReceived
handleRequestWai options req respond actions = do
  -- NOTE: Remember, this is called for both updates AND for page loads
  body <- liftIO $ Wai.consumeRequestBodyLazy req
  rq <- either throwIO pure $ do
    fromWaiRequest req body
  (res, client, rmts) <- runHyperbole rq actions
  liftIO $ sendResponse options rq client res rmts respond


sendResponse :: ServerOptions -> Request -> Client -> Response -> [Remote] -> (Wai.Response -> IO ResponseReceived) -> IO Wai.ResponseReceived
sendResponse options req client res remotes respond = do
  let metas = requestMetadata req <> responseMetadata req.path client remotes
  respond $ response metas res
 where
  response :: Metadata -> Response -> Wai.Response
  response metas = \case
    (Err err) ->
      respondError (errStatus err) [] $ options.serverError err
    (Response _ vw) -> do
      respondHtml status200 (clientHeaders client) $ renderViewResponse metas vw
    (Redirect u) -> do
      let url = uriToText u
      -- We have to use a 200 javascript redirect because javascript
      -- will redirect the fetch(), while we want to redirect the whole page
      -- see index.ts sendAction()
      let hs = ("Location", cs url) : clientHeaders client
      respondHtml status200 hs $ renderViewResponse (metaRedirect u <> metas) $ do
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

  errStatus = \case
    NotFound -> status404
    ErrParse _ -> status400
    ErrQuery _ -> status400
    ErrSession _ _ -> status400
    ErrAuth _ -> status401
    _ -> status500

  -- convert to document if full page request
  addDocument :: BL.ByteString -> BL.ByteString
  addDocument body =
    case req.event of
      Nothing -> options.toDocument body
      _ -> body

  renderViewResponse :: Metadata -> View Body () -> BL.ByteString
  renderViewResponse metas vw =
    addDocument $ renderLazyByteString (addContext metas $ scriptMeta metas) <> "\n\n" <> renderLazyByteString (addContext Body vw)

  respondError s hs serr = respondHtml s hs $ renderViewResponse (metaError serr.message) serr.body
  respondHtml s hs = Wai.responseLBS s (contentType ContentHtml : hs)
  -- respondText s hs = Wai.responseLBS s (contentType ContentText : hs)

  -- via HTTP, we want to manually set some headers rather than just rely on the client
  clientHeaders :: Client -> [Header]
  clientHeaders = setCookies
   where
    setCookies clnt =
      fmap setCookie $ Cookie.toList clnt.session

    setCookie :: Cookie -> (HeaderName, BS.ByteString)
    setCookie cookie =
      ("Set-Cookie", Cookie.render req.path cookie)


scriptMeta :: Metadata -> View Metadata ()
scriptMeta m =
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
      event = Nothing
  cookies <- fromCookieHeader cookie

  pure $
    Request
      { body = body
      , path = pth
      , event
      , query
      , method
      , cookies
      , host
      , requestId
      }


-- where
--  lookupEvent :: [Header] -> Maybe (Event TargetViewId Encoded Encoded)
--  lookupEvent headers = do
--    viewId <- TargetViewId . cs <$> L.lookup "Hyp-ViewId" headers
--    actText <- cs <$> L.lookup "Hyp-Action" headers
--    case encodedParseText actText of
--      Left _ -> Nothing
--      Right a -> pure $ Event viewId a st

-- Client only returns ONE Cookie header, with everything concatenated
fromCookieHeader :: BS.ByteString -> Either MessageError Cookies
fromCookieHeader h =
  case Cookie.parse (Web.Cookie.parseCookies h) of
    Left err -> Left $ InvalidCookie h err
    Right a -> pure a


contentType :: ContentType -> (HeaderName, BS.ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")
