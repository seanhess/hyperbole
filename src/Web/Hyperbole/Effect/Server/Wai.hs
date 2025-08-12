{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.Server.Wai where

import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types (Header, HeaderName, Method, status200, status400, status401, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (path, uriToText)
import Web.Hyperbole.Effect.Page
import Web.Hyperbole.Effect.Server.Response
import Web.Hyperbole.Effect.Server.Types
import Web.Hyperbole.Types.Event (RequestId (..))
import Web.Hyperbole.View (renderLazyByteString)


runPageWai
  :: (IOE :> es)
  => Wai.Request
  -> Eff (Page : es) a
  -> Eff es (Either Interrupt (a, Client))
runPageWai req = reinterpret runLocal $ \_ -> \case
  GetPageInfo -> pure $ waiPageInfo req
  InterruptWith i -> throwError i
  PutClient c -> put c
  GetClient -> get
 where
  runLocal :: (IOE :> es) => Eff (State Client : Error Interrupt : es) a -> Eff es (Either Interrupt (a, Client))
  runLocal eff = do
    runErrorNoCallStack @Interrupt $ do
      clt <- initClient req
      runState clt eff

  cookies :: (Error Interrupt :> es) => Wai.Request -> Eff es Cookies
  cookies wr = do
    let header = fromMaybe "" $ L.lookup "Cookie" $ Wai.requestHeaders wr
    fromCookieHeader header

  initClient :: (Error Interrupt :> es) => Wai.Request -> Eff es Client
  initClient wr = do
    session <- cookies wr
    let query = queryData $ Wai.queryString wr
    pure $ Client{session, query}

  waiPageInfo :: Wai.Request -> PageInfo
  waiPageInfo wr =
    let host = Host $ fromMaybe "" $ L.lookup "Host" $ Wai.requestHeaders wr
        pth = path $ cs $ Wai.rawPathInfo wr
     in PageInfo host pth


-- runServerWai
--   :: (IOE :> es)
--   => (BL.ByteString -> BL.ByteString)
--   -> Wai.Request
--   -> (Wai.Response -> IO ResponseReceived)
--   -> Eff (Server : es) a
--   -> Eff es (Maybe Wai.ResponseReceived)
-- runServerWai toDoc req respond =
--   reinterpret runLocal $ \_ -> \case
--     LoadRequest -> do
--       fromWaiRequest req
--     SendResponse client r -> do
--       rr <- liftIO $ sendResponse client r
--       put (Just rr)
--  -- where
-- runLocal :: (IOE :> es) => Eff (State (Maybe ResponseReceived) : es) a -> Eff es (Maybe ResponseReceived)
-- runLocal = execState Nothing
--
-- sendResponse :: Client -> Response -> IO Wai.ResponseReceived
-- sendResponse client r =
--   respond $ response r
--  where
--   response :: Response -> Wai.Response
--   response NotFound = respError status404 "Not Found"
--   response Empty = respError status500 "Empty Response"
--   response (Err (ErrParse e)) = respError status400 ("Parse Error: " <> cs e)
--   response (Err (ErrQuery e)) = respError status400 $ "ErrQuery: " <> cs e
--   response (Err (ErrSession param e)) = respError status400 $ "ErrSession: " <> cs (show param) <> " " <> cs e
--   response (Err (ErrServer msg)) = do
--     respError status500 $ "Server Error: " <> cs msg
--   response (Err (ErrCustom _ body)) = do
--     let out = addDocument (Wai.requestMethod req) (renderLazyByteString body)
--     Wai.responseLBS status500 [contentType ContentHtml] out
--   response (Err ErrInternal) = respError status500 "Internal Server Error"
--   response (Err (ErrAuth m)) = respError status401 $ "Unauthorized: " <> cs m
--   response (Err (ErrNotHandled e)) = respError status400 $ cs $ errNotHandled e
--   response (Response _ vw) =
--     respHtml $
--       addDocument (Wai.requestMethod req) (renderLazyByteString vw)
--   response (Redirect u) = do
--     let url = uriToText u
--     -- We have to use a 200 javascript redirect because javascript
--     -- will redirect the fetch(), while we want to redirect the whole page
--     -- see index.ts sendAction()
--     let hs = ("Location", cs url) : contentType ContentHtml : headers
--     Wai.responseLBS status200 hs $ "<script>window.location = '" <> cs url <> "'</script>"
--
--   respError s = Wai.responseLBS s [contentType ContentText]
--
--   respHtml body =
--     -- always set the session...
--     let hs = contentType ContentHtml : (setQuery client.query <> headers)
--      in Wai.responseLBS status200 hs body
--
--   headers :: [Header]
--   headers = setRequestId client.requestId : setCookies
--
--   setCookies =
--     fmap setCookie $ Cookie.toList client.session
--
--   setCookie :: Cookie -> (HeaderName, BS.ByteString)
--   setCookie cookie =
--     ("Set-Cookie", Cookie.render (path $ cs $ Wai.rawPathInfo req) cookie)
--
--   setRequestId :: RequestId -> (HeaderName, BS.ByteString)
--   setRequestId (RequestId rid) =
--     ("Request-Id", cs rid)
--
--   setQuery Nothing = []
--   setQuery (Just qd) =
--     [("Set-Query", QueryData.render qd)]
--
-- -- convert to document if full page request. Subsequent POST requests will only include fragments
-- addDocument :: Method -> BL.ByteString -> BL.ByteString
-- addDocument "GET" bd = toDoc bd
-- addDocument _ bd = bd

-- fromWaiRequest :: (MonadIO m) => Wai.Request -> m Request
-- fromWaiRequest wr = do
--   body <- liftIO $ Wai.consumeRequestBodyLazy wr
--   let pth = path $ cs $ Wai.rawPathInfo wr
--       query = Wai.queryString wr
--       headers = Wai.requestHeaders wr
--       cookie = fromMaybe "" $ L.lookup "Cookie" headers
--       host = Host $ fromMaybe "" $ L.lookup "Host" headers
--       requestId = RequestId $ cs $ fromMaybe "" $ L.lookup "Request-Id" headers
--       method = Wai.requestMethod wr
--
--   cookies <- fromCookieHeader cookie
--
--   pure $ Request{body, path = pth, query, method, cookies, host, requestId}

-- Client only returns ONE Cookie header, with everything concatenated
fromCookieHeader :: (Error Interrupt :> es) => BS.ByteString -> Eff es Cookies
fromCookieHeader h =
  case Cookie.parse (Web.Cookie.parseCookies h) of
    -- TODO: send an invalid client response, this is bad!
    Left err -> throwError $ Err $ InvalidCookies (cs err) h
    Right a -> pure a


-- errNotHandled :: Event TargetViewId Text -> String
-- errNotHandled ev =
--   L.intercalate
--     "\n"
--     [ "No Handler for Event viewId: " <> cs ev.viewId.text <> " action: " <> cs ev.action
--     , "<p>Remember to add a `hyper` handler in your page function</p>"
--     , "<pre>"
--     , "page :: (Hyperbole :> es) => Page es Response"
--     , "page = do"
--     , "  handle contentsHandler"
--     , "  load $ do"
--     , "    pure $ hyper Contents contentsView"
--     , "</pre>"
--     ]

contentType :: ContentType -> (HeaderName, BS.ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")


data ContentType
  = ContentHtml
  | ContentText
