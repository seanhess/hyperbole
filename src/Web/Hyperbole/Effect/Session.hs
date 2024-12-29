module Web.Hyperbole.Effect.Session where

import Data.ByteString (ByteString)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text, pack)
import Network.HTTP.Types
import Text.Read (readMaybe)
import Web.HttpApiData
import Prelude


newtype Session = Session (Map Text Text)
  deriving (Show)


-- | Set the session key to value
sessionSet :: (ToHttpApiData a) => Text -> a -> Session -> Session
sessionSet k a (Session kvs) =
  let val = toQueryParam a
   in Session $ Map.insert k val kvs


sessionDel :: Text -> Session -> Session
sessionDel k (Session kvs) =
  Session $ Map.delete k kvs


sessionLookup :: (FromHttpApiData a) => Text -> Session -> Maybe a
sessionLookup k (Session sm) = do
  t <- Map.lookup k sm
  either (const Nothing) pure $ parseQueryParam t


sessionEmpty :: Session
sessionEmpty = Session Map.empty


-- | Render a session as a url-encoded query string
sessionRender :: Session -> ByteString
sessionRender (Session sm) =
  urlEncode True $ renderQuery False (toQuery $ Map.toList sm)


-- | Parse a session as a url-encoded query string
sessionParse :: ByteString -> Session
sessionParse = Session . Map.fromList . map toText . parseQuery . urlDecode True
 where
  toText (k, Nothing) = (cs k, "false")
  toText (k, Just v) = (cs k, cs v)


sessionFromCookies :: [(ByteString, ByteString)] -> Session
sessionFromCookies cks = fromMaybe sessionEmpty $ do
  bs <- L.lookup "session" cks
  pure $ sessionParse bs


sessionSetCookie :: Session -> ByteString
sessionSetCookie ss = "session=" <> sessionRender ss <> "; SameSite=None; secure; path=/"


showQueryParam :: (Show a) => a -> Text
showQueryParam a = toQueryParam $ show a


readQueryParam :: (Read a) => Text -> Either Text a
readQueryParam t = do
  str <- parseQueryParam t
  case readMaybe str of
    Nothing -> Left $ pack $ "Could not read HttpApiData: " <> str
    Just a -> pure a
