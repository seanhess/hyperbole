{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effect.Server.Socket where

import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.QueryData as QueryData
import Web.Hyperbole.Data.URI (URI, uriToText)
import Web.Hyperbole.Effect.Server.Response
import Web.Hyperbole.Effect.Server.Types
import Web.Hyperbole.View (View, renderLazyByteString)


runServerSockets
  :: (IOE :> es)
  => Connection
  -> Request
  -> Eff (Server : es) Response
  -> Eff es Response
runServerSockets conn req = reinterpret runLocal $ \_ -> \case
  LoadRequest -> pure req -- receiveRequest conn
  SendResponse client res -> do
    case res of
      (Response vid vw) -> do
        sendView req conn client vid vw
      (Err (ErrServer m)) -> sendError req conn (serverError m)
      (Err err) -> sendError req conn (serializeError err)
      Empty -> sendError req conn (serverError "Empty")
      NotFound -> sendError req conn (serverError "Not Found")
      (Redirect url) -> sendRedirect req conn client url
 where
  runLocal = runErrorNoCallStackWith @SocketError onSocketError

  errMsg (ErrServer m) = m
  errMsg ErrInternal = "Internal Server Error"
  errMsg e = pack (drop 3 $ show e)

  serializeError (ErrCustom m b) = SerializedError m (renderLazyByteString b)
  serializeError err = serverError $ errMsg err

  onSocketError :: (IOE :> es) => SocketError -> Eff es Response
  onSocketError e = do
    let msg = cs $ show e
    sendError req conn $ serverError msg
    pure $ Err $ ErrServer msg


sendView :: (IOE :> es) => Request -> Connection -> Client -> TargetViewId -> View () () -> Eff es ()
sendView req conn client tv vw = do
  sendResponse req conn client (viewIdMeta tv) (renderLazyByteString vw)
 where
  viewIdMeta :: TargetViewId -> Metadata
  viewIdMeta (TargetViewId vid) = Metadata [("VIEW-ID", cs vid)]


sendRedirect :: (IOE :> es) => Request -> Connection -> Client -> URI -> Eff es ()
sendRedirect req conn client u = do
  sendResponse req conn client (metadata "REDIRECT" (uriToText u)) ""


-- send response with client metadata
sendResponse :: (MonadIO m) => Request -> Connection -> Client -> Metadata -> BL.ByteString -> m ()
sendResponse req conn client meta =
  sendMessage conn (responseMeta <> meta)
 where
  responseMeta :: Metadata
  responseMeta =
    metaRequestId client.requestId <> metaSession client.session <> metaQuery client.query

  metaSession :: Cookies -> Metadata
  metaSession cookies = mconcat $ fmap metaCookie $ Cookie.toList cookies
   where
    metaCookie :: Cookie -> Metadata
    metaCookie cookie =
      Metadata [("COOKIE", cs (Cookie.render req.path cookie))]


sendError :: (IOE :> es) => Request -> Connection -> SerializedError -> Eff es ()
sendError req conn (SerializedError err body) = do
  sendMessage conn (metaRequestId req.requestId <> metadata "ERROR" err) body


-- low level message. Use sendResponse
sendMessage :: (MonadIO m) => Connection -> Metadata -> BL.ByteString -> m ()
sendMessage conn meta' cnt = do
  let msg = renderMetadata meta' <> "\n" <> cnt
  liftIO $ WS.sendTextData conn msg
 where
  renderMetadata :: Metadata -> BL.ByteString
  renderMetadata (Metadata m) = BL.intercalate "\n" $ fmap (uncurry metaLine) m

  metaLine :: BL.ByteString -> Text -> BL.ByteString
  metaLine name value = "|" <> name <> "|" <> cs value


-- Metadata --------------------------------------------

metadata :: BL.ByteString -> Text -> Metadata
metadata name value = Metadata [(name, value)]


metaRequestId :: RequestId -> Metadata
metaRequestId (RequestId reqId) =
  Metadata [("REQUEST-ID", cs reqId)]


metaQuery :: Maybe QueryData -> Metadata
metaQuery Nothing = mempty
metaQuery (Just q) =
  Metadata [("QUERY", cs $ QueryData.render q)]
