module Web.Hyperbole.Application
  ( waiApplication
  , Wai
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.String.Conversions (cs)
import Effectful
import Effectful.Wai
import Network.HTTP.Types (status200, status301, status400, status404)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai
import Web.Hyperbole.Route
import Web.View.Types (Url (..))


waiApplication :: (Route route) => (L.ByteString -> L.ByteString) -> (route -> Eff [Wai, IOE] ()) -> Application
waiApplication toDoc actions request respond = do
  -- let (method, paths, query) = (requestMethod req, pathInfo req, queryString req)
  case findRoute (pathInfo request) of
    Nothing -> respond $ responseLBS status404 [contentType ContentText] "Not Found"
    Just rt -> do
      res <- runEff . runWai request $ actions rt
      case res of
        Left err -> interrupt err
        Right resp -> sendResponse resp
 where
  findRoute [] = Just defRoute
  findRoute ps = matchRoute (Path True ps)

  -- convert to document if GET. Subsequent POST requests will only include fragments
  addDocument "GET" bd = toDoc bd
  addDocument _ bd = bd

  contentType :: ContentType -> (HeaderName, ByteString)
  contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
  contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")

  sendResponse :: Handler -> IO ResponseReceived
  sendResponse resp = do
    let headers = contentType resp.contentType : resp.headers
        respBody = addDocument (requestMethod request) resp.body
    respond $ responseLBS status200 headers respBody

  interrupt NotFound =
    respond $ responseLBS status404 [contentType ContentText] "Not Found"
  interrupt (ParseError e) = do
    -- TODO: logging!
    putStrLn $ "Parse Error: " <> cs e
    respond $ responseLBS status400 [contentType ContentText] $ "Parse Error: " <> cs e
  interrupt (Redirect (Url u)) =
    respond $ responseLBS status301 [("Location", cs u)] ""
  interrupt (RespondNow resp) = do
    sendResponse resp


{- | Run both the http and ws application
application :: (Route route) => (L.ByteString -> L.ByteString) -> (route -> Eff [Wai, IOE] ()) -> Application
application toDoc actions =
  websocketsOr connectionOptions (socketApplication talk)
    $ httpApplication toDoc actions
-}

-- talk :: (Socket :> es) => Eff es ()
-- talk = do
--   msg :: Text <- receiveData
--
--   -- HANDLE ACTION?
--   -- SEND VIEW (possibly multiple times). Need to run action...
--
--   sendCommand $ Render [Text $ "Updated: " <> msg]
