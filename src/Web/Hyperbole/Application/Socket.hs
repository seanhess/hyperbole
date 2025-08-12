{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Application.Socket where

import Control.Applicative (many)
import Data.Attoparsec.Text (Parser, char, endOfLine, isEndOfLine, takeText, takeTill, takeWhile1)
import Data.Attoparsec.Text qualified as Atto
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception (Exception, throwIO)
import Effectful.State.Static.Local
import GHC.Generics
import Network.HTTP.Types as HTTP (parseQuery)
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Cookie qualified
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (Path, URI, path, uriToText)
import Web.Hyperbole.Effect.Page (Client (..), PageError (..), PageInfo (..))
import Web.Hyperbole.Effect.Server.Response
import Web.Hyperbole.Effect.Server.Types
import Web.Hyperbole.Types.Error (SerializedError (..))
import Web.Hyperbole.Types.Event (Event (..), RequestId (..), TargetViewId (..))
import Web.Hyperbole.View (View, renderLazyByteString, renderText)


data SocketError
  = InvalidMessageType Text
  | MissingMeta Text
  | MessageParseFail String
  deriving (Show, Exception)


receiveRequest :: (IOE :> es, Error SocketError :> es, Error PageError :> es) => Connection -> Eff es SocketRequest
receiveRequest conn = do
  -- TODO: catch and lift errors here
  input <- liftIO $ WS.receiveData @Text conn
  msg <- runParseMessage input
  fromMessage msg


runParseMessage :: (Error SocketError :> es) => Text -> Eff es Message
runParseMessage input =
  case Atto.parseOnly parseMessage input of
    Left e -> throwError $ MessageParseFail e
    Right m -> pure m


parseMessage :: Parser Message
parseMessage = do
  (typ, info) <- parseTypeInfo
  meta <- many parseMetaLine
  body <- parseBody
  pure $ Message typ info meta body
 where
  parseTypeInfo :: Parser (Text, Text)
  parseTypeInfo = do
    _ <- char '|'
    typ <- takeWhile1 (\c -> c /= '|' && c /= '\n')
    _ <- char '|'
    info <- takeWhile1 (not . isEndOfLine)
    pure (typ, info)

  parseMetaLine :: Parser (Text, Text)
  parseMetaLine = do
    k <- takeWhile1 (\c -> c /= ':' && c /= '\n')
    _ <- char ':'
    v <- takeTill (== '\n')
    _ <- endOfLine
    pure (k, v)

  parseBody :: Parser (Maybe Text)
  parseBody = do
    t <- takeText
    case T.stripStart t of
      "" -> pure Nothing
      _ -> pure $ Just t


-- NOTE: PageInfo comes from the websocket connection itself
-- actions do need to send the client though
data SocketRequest
  = RunAction Client (Event TargetViewId Text)


fromMessage :: (Error SocketError :> es, Error PageError :> es) => Message -> Eff es SocketRequest
fromMessage (Message typ _ meta _) =
  case typ of
    "UPDATE" -> do
      ev <- eventMeta
      cl <- clientMeta
      pure $ RunAction cl ev
    _ -> throwError $ InvalidMessageType typ
 where
  eventMeta :: (Error SocketError :> es) => Eff es (Event TargetViewId Text)
  eventMeta = do
    vid <- metaKey "viewId"
    act <- metaKey "action"
    eid <- metaKey "requestId"
    pure $ Event (TargetViewId vid) act (RequestId eid)

  clientMeta :: (Error SocketError :> es, Error PageError :> es) => Eff es Client
  clientMeta = do
    session <- cookies =<< metaKey "session"
    query <- queryData . HTTP.parseQuery . cs <$> metaKey "query"
    pure $ Client{session, query}

  cookies :: (Error SocketError :> es, Error PageError :> es) => Text -> Eff es Cookies
  cookies val = do
    let cooks = cs val
    case Cookie.parse $ Web.Cookie.parseCookies cooks of
      Left e -> throwError $ InvalidCookies (cs e) cooks
      Right a -> pure a

  metaKey :: (Error SocketError :> es) => Text -> Eff es Text
  metaKey k = do
    case L.lookup k meta of
      Nothing -> throwError $ MissingMeta k
      Just val -> pure val


-- parseUrl :: Text -> Either SocketError (Text, Text)
-- parseUrl u =
--   case T.splitOn "?" u of
--     [url, query] -> pure (url, query)
--     _ -> Left $ InvalidMessage u

-- parseValues :: Text -> Text -> Text -> Text -> Maybe Text -> Either SocketError Request
-- parseValues url cook hst reqId mbody = do
--   (u, q) <- parseUrl url
--   let pth = path u
--       query = HTTP.parseQuery (cs q)
--       host = Host $ cs $ header hst
--       method = "POST"
--       body = cs $ fromMaybe "" mbody
--       requestId = RequestId $ header reqId
--
--   cookies <- first (InternalSocket . InvalidCookie (cs cook)) <$> Cookie.parse $ Web.Cookie.parseCookies $ cs $ header cook
--
--   pure $ Request{path = pth, host, query, body, method, cookies, requestId}

-- NOTE: RAW PROTCOL
-- 1. only one message type
-- 2. parse into strict types on the client
-- 3. can have body
-- 4. can have metadata

{-
 |UPDATE|SomeViewId
 requestId:alkjasf

 some text
 does anything need the body besides?
-}

{-
 |QUERY|key=value
-}

{-
 |COOKIE|key=value
-}

data Message = Message
  { msgType :: Text
  , msgInfo :: Text
  , msgMeta :: [(Text, Text)]
  , msgBody :: Maybe Text
  }


sendRawMessage :: (MonadIO m) => Connection -> Message -> m ()
sendRawMessage conn msg = do
  liftIO $ WS.sendTextData conn $ rawMessage msg
 where
  rawMessage :: Message -> Text
  rawMessage (Message typ info meta body) =
    let lineMain = "|" <> typ <> "|" <> cs info
        linesMeta = T.intercalate "\n" $ fmap metaLine meta
        linesBody = maybe "" ("\n\n" <>) body
     in T.intercalate "\n" [lineMain, linesMeta, linesBody]

  metaLine :: (Text, Text) -> Text
  metaLine (k, v) = k <> ": " <> v


sendView :: (IOE :> es) => Connection -> TargetViewId -> View () () -> Eff es ()
sendView conn (TargetViewId vid) vw = do
  let RequestId rid = RequestId "reqid?"
  sendRawMessage conn $ Message "UPDATE" vid [("requestId", rid)] (Just $ renderText vw)


sendRedirect :: (IOE :> es) => Connection -> URI -> Eff es ()
sendRedirect conn u = do
  sendRawMessage conn $ Message "REDIRECT" (uriToText u) [] Nothing


sendQuery :: (IOE :> es) => Connection -> QueryData -> Eff es ()
sendQuery conn q = do
  sendRawMessage conn $ Message "QUERY" (cs $ QueryData.render q) [] Nothing


sendError :: (IOE :> es) => Connection -> SerializedError -> Eff es ()
sendError conn (SerializedError err body) = do
  sendRawMessage conn $ Message "ERROR" err [] (Just $ cs body)

----------------

-- data SocketRequest = SocketRequest
--   { request :: Maybe Request
--   }

-- runServerSockets
--   :: (IOE :> es)
--   => Connection
--   -> Eff (Server : es) a
--   -> Eff es a
-- runServerSockets conn = reinterpret runLocal $ \_ -> \case
--   -- load the request
--   LoadRequest -> do
--     loadRequest
--   SendResponse client res -> do
--     req <- loadRequest
--     -- cannot be called before loading the request!
--     case res of
--       (Response vid vw) -> do
--         sendView req.path conn client vid vw
--       (Err (ErrServer m)) -> sendError req.requestId conn (serverError m)
--       (Err err) -> sendError req.requestId conn (serializeError err)
--       Empty -> sendError req.requestId conn (serverError "Empty")
--       NotFound -> sendError req.requestId conn (serverError "Not Found")
--       (Redirect url) -> sendRedirect req.path conn client url
--  where
--   runLocal :: Eff (State SocketRequest : es) a -> Eff es a
--   runLocal = evalState (SocketRequest Nothing)
--
--   loadRequest :: (State SocketRequest :> es, IOE :> es) => Eff es Request
--   loadRequest = do
--     sock <- get @SocketRequest
--     case sock.request of
--       -- return the existing request, don't wait for another one!
--       Just r -> pure r
--       -- load the request for the first time
--       Nothing -> do
--         req <- receiveRequest conn
--         put $ SocketRequest (Just req)
--         pure req
--
--   errMsg (ErrServer m) = m
--   errMsg ErrInternal = "Internal Server Error"
--   errMsg e = pack (drop 3 $ show e)
--
--   serializeError (ErrCustom m b) = SerializedError m (renderLazyByteString b)
--   serializeError err = serverError $ errMsg err
--
--   -- onSocketError :: (IOE :> es) => SocketError -> Eff es Response
--   -- onSocketError e = do
--   --   let msg = cs $ show e
--   --   sendError req conn $ serverError msg
--   --   pure $ Err $ ErrServer msg
--
--   receiveRequest :: (IOE :> es) => Connection -> Eff es Request
--   receiveRequest _ = do
--     t <- receiveText conn
--     case parseMessage t of
--       Left e -> throwIO e
--       Right r -> pure r
--
--   receiveText :: (IOE :> es) => Connection -> Eff es Text
--   receiveText _ = do
--     -- c <- ask @Connection
--     liftIO $ WS.receiveData conn
