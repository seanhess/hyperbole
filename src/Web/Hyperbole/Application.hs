module Web.Hyperbole.Application
  ( waiApplication
  -- , webSocketApplication
  , application
  , websocketsOr
  ) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Network.HTTP.Types (Method, status200, status400, status404)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Hyperbole.Effect
import Web.Hyperbole.Route
import Web.View (renderLazyByteString)


waiApplication :: (Route route) => (L.ByteString -> L.ByteString) -> (route -> Eff '[Hyperbole, IOE] ()) -> Wai.Application
waiApplication toDoc actions request respond = do
  req <- toRequest request
  case findRoute req.path of
    -- Nothing -> respond $ responseLBS status404 [contentType ContentText] "Not Found"
    Nothing -> respond $ Wai.responseLBS status404 [] "Not Found"
    Just rt -> do
      res <- runEff . runHyperbole req $ actions rt
      sendResponse res
 where
  -- TODO: build this functionality into the main effect handler?
  findRoute [] = Just defRoute
  findRoute ps = matchRoute (Path True ps)

  toRequest req = do
    bd <- liftIO $ Wai.consumeRequestBodyLazy req
    pure
      $ Request
        { body = bd
        , path = Wai.pathInfo req
        , query = Wai.queryString req
        }

  -- convert to document if GET. Subsequent POST requests will only include fragments
  addDocument :: Method -> L.ByteString -> L.ByteString
  addDocument "GET" bd = toDoc bd
  addDocument _ bd = bd

  -- TODO: logging!
  sendResponse :: Response -> IO Wai.ResponseReceived
  sendResponse (ErrParse e) = do
    putStrLn $ "Parse Error: " <> cs e
    respond $ Wai.responseLBS status400 [contentType ContentText] $ "Parse Error: " <> cs e
  sendResponse ErrNoHandler = do
    putStrLn "No Handler Found"
    respond $ Wai.responseLBS status400 [contentType ContentText] "No Handler Found"
  sendResponse (Response vw) = do
    -- let headers = contentType resp.contentType : resp.headers
    let headers = [contentType ContentHtml]
        respBody = addDocument (Wai.requestMethod request) (renderLazyByteString vw)
    respond $ Wai.responseLBS status200 headers respBody


data ContentType
  = ContentHtml
  | ContentText


contentType :: ContentType -> (HeaderName, ByteString)
contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")


-- interrupt NotFound =
--   respond $ responseLBS status404 [contentType ContentText] "Not Found"
-- interrupt (ParseError e) = do
-- -- interrupt (Redirect (Url u)) =
-- --   respond $ responseLBS status301 [("Location", cs u)] ""
-- interrupt (RespondNow resp) = do
--   sendResponse resp

application :: (Route route) => (L.ByteString -> L.ByteString) -> (route -> Eff '[Hyperbole, IOE] ()) -> Wai.Application
application toDoc actions =
  websocketsOr opts socketApp
    $ waiApplication toDoc actions
 where
  opts = defaultConnectionOptions

  socketApp :: PendingConnection -> IO ()
  socketApp pending = do
    conn <- WS.acceptRequest pending
    -- WS.sendTextData conn ("HELLO CLIENT" :: Text)
    forever $ talk conn

  talk :: Connection -> IO ()
  talk conn = do
    -- 1. Receive data
    -- 2. Parse Request
    -- 3. Send Response
    print @String "TALK"
    (t :: Text) <- receive
    print $ "Received: " <> t
    send $ "Received: " <> t
   where
    receive = WS.receiveData conn
    send = WS.sendTextData conn

-- receiveTextData :: Connection -> IO L.ByteString
-- receiveTextData conn = do
--   msg <- receiveDataMessage conn
--   case msg of
--     Text bs _ -> pure bs
--     Binary bs -> fail $ "Received unexpected binary data: " <> show (L.length bs)
