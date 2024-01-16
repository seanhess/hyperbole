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
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Network.HTTP.Types (Method, Query, parseQuery, status200, status400, status404)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, PendingConnection, defaultConnectionOptions)
import Network.WebSockets qualified as WS
import Web.Hyperbole.Effect
import Web.Hyperbole.Route
import Web.View (View, renderLazyByteString)


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


-- TODO: build this functionality into the main effect handler?
findRoute [] = Just defRoute
findRoute ps = matchRoute (Path True ps)


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
    -- need to route as well!
    res <- runSocket $ do
      liftIO $ print @String "TALK"
      req <- request
      liftIO $ print req

      case findRoute req.path of
        Nothing -> throwError RouteNotFound
        Just rt -> do
          -- I'm already in the other effect stack, but we can run IO anywhere!
          liftIO $ runEff . runHyperbole req $ actions rt

    case res of
      Right (ErrParse t) -> sendError t
      Right ErrNoHandler -> sendError @Text "ErrNoHandler"
      Right NotFound -> sendError @Text "NotFound"
      Right (Response vw) -> sendView vw
      Left err -> sendError err

    pure ()
   where
    -- run everything in Error String

    runSocket :: Eff '[Error SocketError, Reader Connection, IOE] Response -> IO (Either SocketError Response)
    runSocket = runEff . runReader conn . runErrorNoCallStack @SocketError

    request :: (IOE :> es, Reader Connection :> es, Error SocketError :> es) => Eff es Request
    request = do
      t <- receive
      case parseMessage t of
        Left e -> throwError e
        Right r -> pure r

    receive :: (Reader Connection :> es, IOE :> es) => Eff es Text
    receive = do
      c <- ask @Connection
      liftIO $ WS.receiveData c

    parseMessage :: Text -> Either SocketError Request
    parseMessage t = do
      (url, body) <- messageParts t
      (path, query) <- urlParts url
      pure $ Request path query (cs body)

    messageParts :: Text -> Either SocketError (Text, Text)
    messageParts t = do
      case T.splitOn "\n" t of
        [url, body] -> pure (url, body)
        _ -> Left $ InvalidMessage t

    urlParts :: Text -> Either SocketError ([Text], Query)
    urlParts t = do
      case T.splitOn "?" t of
        [path, query] -> pure (T.splitOn "/" path, parseQuery (cs query))
        _ -> Left $ InvalidMessage t

    sendView :: View () () -> IO ()
    sendView vw = WS.sendTextData conn $ renderLazyByteString vw

    sendError :: (Show e) => e -> IO ()
    sendError e = WS.sendTextData conn $ pack (show e)


data SocketError
  = InvalidMessage Text
  | RouteNotFound
  deriving (Show, Eq)
