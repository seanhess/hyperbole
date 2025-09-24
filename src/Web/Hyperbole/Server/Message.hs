{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Server.Message where

import Control.Exception (Exception)
import Data.Aeson qualified as Aeson
import Data.Attoparsec.Text (Parser, char, endOfLine, isEndOfLine, parseOnly, sepBy, string, takeText, takeTill, takeWhile1)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Web.Hyperbole.Data.Cookie (Cookie, Cookies)
import Web.Hyperbole.Data.Cookie qualified as Cookie
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Data.QueryData (QueryData)
import Web.Hyperbole.Data.QueryData qualified as QueryData
import Web.Hyperbole.Data.URI (Path, URI, uriToText)
import Web.Hyperbole.Effect.Hyperbole (Remote (..))
import Web.Hyperbole.Types.Client (Client (..))
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request


{-
 |UPDATE|
 viewId: wahoo
 action: hello
 requestId: ipgeim

 body
 body
 body
-}

data Message = Message
  { messageType :: Text
  , event :: Event TargetViewId Encoded
  , requestId :: RequestId
  , metadata :: Metadata
  , body :: MessageBody
  }
  deriving (Show)


newtype MessageBody = MessageBody {value :: BL.ByteString}
  deriving newtype (Show)


data MessageError
  = InvalidMessage String Text
  | InvalidCookie BS.ByteString String
  | MissingMeta String
  deriving (Show, Exception)


-- Read Messages -------------------------------------

mimeType :: Text
mimeType = "application/hyperbole.message"


parseActionMessage :: Text -> Either String Message
parseActionMessage = parseOnly parser
 where
  parser :: Parser Message
  parser = do
    mt <- messageType
    ev <- event
    rq <- requestId
    ms <- meta `sepBy` endOfLine
    bd <- body
    pure $ Message mt ev rq (mconcat ms) bd

  messageType :: Parser Text
  messageType = do
    _ <- char '|'
    t <- takeWhile1 (/= '|')
    _ <- char '|'
    endOfLine
    pure t

  body :: Parser MessageBody
  body = do
    MessageBody . cs . T.strip <$> takeText

  event :: Parser (Event TargetViewId Encoded)
  event = do
    vid <- targetViewId
    endOfLine
    act <- encodedAction
    endOfLine
    pure $ Event vid act
   where
    targetViewId :: Parser TargetViewId
    targetViewId = do
      _ <- string "ViewId: "
      TargetViewId <$> takeLine

    encodedAction :: Parser Encoded
    encodedAction = do
      _ <- string "Action: "
      inp <- takeLine
      case encodedParseText inp of
        Left e -> fail $ "Parse Encoded ViewAction failed: " <> cs e <> " from " <> cs inp
        Right a -> pure a

  requestId :: Parser RequestId
  requestId = do
    _ <- string "RequestId: "
    r <- RequestId <$> takeLine
    endOfLine
    pure r

  meta :: Parser Metadata
  meta = do
    key <- metaKey
    value <- takeLine
    pure $ metadata (cs key) value

  metaKey :: Parser MetaKey
  metaKey = do
    key <- takeWhile1 (/= ':')
    _ <- string ": "
    pure key

  takeLine :: Parser Text
  takeLine = do
    takeTill isEndOfLine


-- Render ---------------------------------------------

renderMetadata :: Metadata -> Text
renderMetadata (Metadata m) = T.intercalate "\n" $ fmap (uncurry metaLine) m


metaLine :: MetaKey -> Text -> Text
metaLine name value = name <> ": " <> cs value


-- Metadata --------------------------------------------

type MetaKey = Text


newtype Metadata = Metadata [(Text, Text)]
  deriving newtype (Semigroup, Monoid)
  deriving (Show)


metadata :: MetaKey -> Text -> Metadata
metadata key value = Metadata [(key, value)]


lookupMetadata :: MetaKey -> Metadata -> Maybe Text
lookupMetadata key (Metadata kvs) = L.lookup key kvs


requestMetadata :: Request -> Metadata
requestMetadata req =
  maybe mempty eventMetadata req.event <> metaRequestId req.requestId
 where
  metaRequestId :: RequestId -> Metadata
  metaRequestId (RequestId "") = mempty
  metaRequestId (RequestId reqId) =
    metadata "RequestId" (cs reqId)

  eventMetadata :: Event TargetViewId Encoded -> Metadata
  eventMetadata event =
    Metadata
      [ ("ViewId", event.viewId.text)
      , ("Action", encodedToText event.action)
      ]


responseMetadata :: Path -> Client -> [Remote] -> Metadata
responseMetadata reqPath client remotes =
  clientMetadata reqPath client <> metaRemotes remotes


clientMetadata :: Path -> Client -> Metadata
clientMetadata reqPath client =
  metaSession client.session <> metaQuery client.query <> metaPageTitle client.pageTitle
 where
  metaPageTitle :: Maybe Text -> Metadata
  metaPageTitle = \case
    Nothing -> mempty
    Just pt -> metadata "PageTitle" pt

  metaQuery :: Maybe QueryData -> Metadata
  metaQuery Nothing = mempty
  metaQuery (Just q) =
    Metadata [("Query", cs $ QueryData.render q)]

  metaSession :: Cookies -> Metadata
  metaSession cookies = mconcat $ fmap metaCookie $ Cookie.toList cookies
   where
    metaCookie :: Cookie -> Metadata
    metaCookie cookie =
      Metadata [("Cookie", cs (Cookie.render reqPath cookie))]


metaRemotes :: [Remote] -> Metadata
metaRemotes rs = mconcat $ fmap meta rs
 where
  meta = \case
    RemoteAction (TargetViewId vid) act ->
      metadata "Trigger" $ vid <> "|" <> encodedToText act
    RemoteEvent ev dat ->
      metadata "Event" $ T.intercalate "|" [ev, cs $ Aeson.encode dat]


metaError :: Text -> Metadata
metaError = metadata "Error"


metaRedirect :: URI -> Metadata
metaRedirect u = metadata "Redirect" (uriToText u)


data ContentType
  = ContentHtml
  | ContentText


newtype RenderedHtml = RenderedHtml BL.ByteString
  deriving newtype (Semigroup, Monoid)
