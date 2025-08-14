{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Server.Message where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.Encoded (encodedToText)
import Web.Hyperbole.Data.QueryData (QueryData)
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Data.URI (Path, URI, uriToText)
import Web.Hyperbole.Effect.Hyperbole (Remote (..))
import Web.Hyperbole.Types.Client (Client (..))
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request


data ContentType
  = ContentHtml
  | ContentText


data UpdateMessage = UpdateMessage
  {
  }


newtype RenderedHtml = RenderedHtml BL.ByteString
  deriving newtype (Semigroup, Monoid)


-- Render ---------------------------------------------

renderMetadata :: Metadata -> BL.ByteString
renderMetadata (Metadata m) = BL.intercalate "\n" $ fmap (uncurry metaLine) m


metaLine :: BL.ByteString -> Text -> BL.ByteString
metaLine name value = "|" <> name <> "|" <> cs value


-- Metadata --------------------------------------------

newtype Metadata = Metadata [(BL.ByteString, Text)] deriving newtype (Semigroup, Monoid)


metadata :: BL.ByteString -> Text -> Metadata
metadata name value = Metadata [(name, value)]


requestMetadata :: Request -> Metadata
requestMetadata req =
  let target = (.viewId) <$> req.event
   in maybe mempty metaViewId target <> metaRequestId req.requestId
 where
  metaRequestId :: RequestId -> Metadata
  metaRequestId (RequestId "") = mempty
  metaRequestId (RequestId reqId) =
    Metadata [("REQUEST-ID", cs reqId)]

  metaViewId :: TargetViewId -> Metadata
  metaViewId (TargetViewId v) = metadata "VIEW-ID" v


responseMetadata :: Path -> Client -> [Remote] -> Metadata
responseMetadata reqPath client remotes =
  clientMetadata reqPath client <> metaRemotes remotes


clientMetadata :: Path -> Client -> Metadata
clientMetadata reqPath client =
  metaSession client.session <> metaQuery client.query
 where
  metaQuery :: Maybe QueryData -> Metadata
  metaQuery Nothing = mempty
  metaQuery (Just q) =
    Metadata [("QUERY", cs $ QueryData.render q)]

  metaSession :: Cookies -> Metadata
  metaSession cookies = mconcat $ fmap metaCookie $ Cookie.toList cookies
   where
    metaCookie :: Cookie -> Metadata
    metaCookie cookie =
      Metadata [("COOKIE", cs (Cookie.render reqPath cookie))]


metaRemotes :: [Remote] -> Metadata
metaRemotes rs = mconcat $ fmap meta rs
 where
  meta = \case
    RemoteAction (TargetViewId vid) act ->
      metadata "TRIGGER" $ vid <> "|" <> encodedToText act
    RemoteEvent mvid ev dat ->
      metadata "EVENT" $ T.intercalate "|" [eventTarget mvid, ev, cs $ Aeson.encode dat]

  eventTarget = \case
    Just (TargetViewId t) -> t
    Nothing -> ""


metaError :: Text -> Metadata
metaError = metadata "ERROR"


metaRedirect :: URI -> Metadata
metaRedirect u = metadata "REDIRECT" (uriToText u)
