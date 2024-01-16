module Web.Hyperbole.Application
  ( waiApplication
  -- , webSocketApplication
  , application
  , websocketsOr
  , Wai
  ) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.String.Conversions (cs)
import Effectful
import Effectful.Wai
import Network.HTTP.Types (status200, status301, status400, status404)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Web.Hyperbole.Effect (Hyperbole, runHyperboleWai)
import Web.Hyperbole.Route
import Web.Hyperbole.Socket
import Web.View.Types (Url (..))


waiApplication :: (Route route) => (L.ByteString -> L.ByteString) -> (route -> Eff es ()) -> Application
waiApplication toDoc actions request respond = do
  -- let (method, paths, query) = (requestMethod req, pathInfo req, queryString req)
  case findRoute (pathInfo request) of
    Nothing -> respond $ responseLBS status404 [contentType ContentText] "Not Found"
    Just rt -> do
      res <- runEff . runWai request . runHyperboleWai $ actions rt
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


application :: (Route route, Hyperbole :> es) => (L.ByteString -> L.ByteString) -> (route -> Eff es ()) -> Application
application toDoc actions =
  websocketsOr opts (runSocketApp $ talk)
    $ waiApplication toDoc actions
 where
  opts = defaultConnectionOptions

  talk :: (Socket :> es, IOE :> es, Hyperbole :> es) => Eff es ()
  talk = do
    liftIO $ print @String "TALK"
    t <- receiveData
    liftIO $ print $ "Received: " <> t
    sendMessage $ "Received: " <> t


-- but.... it's not a request response thing...
-- we need to know how to map the route function to something that can produce output
-- maybe we need something lower-level
-- a call/response cycle for each?
-- we need to abstract it, using only ONE effect, or an IO function
-- then map to that one
-- I would LOVE to be able to write my code in an abstraction, but .... might be hard
-- I need a common abstraction.... the read/write loop. Look at wai?
-- they need to be able to handle errors, etc
-- Wai is the wrong name for it. Modify wai to be both!
-- Request?
runToSockets :: (Hyperbole :> es, Route route) => (route -> Eff es ()) -> IO ()
runToSockets = undefined

-- receiveTextData :: Connection -> IO L.ByteString
-- receiveTextData conn = do
--   msg <- receiveDataMessage conn
--   case msg of
--     Text bs _ -> pure bs
--     Binary bs -> fail $ "Received unexpected binary data: " <> show (L.length bs)
