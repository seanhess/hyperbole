module Web.Hyperbole.Session where

import Data.ByteString (ByteString)
import Data.List qualified as L
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types
import Web.HttpApiData


newtype Session = Session [(Text, Text)]
  deriving (Show)
  deriving newtype (QueryLike)


-- I want to serialize this as a query string
setSession :: (ToHttpApiData a) => Text -> a -> Session -> Session
setSession k a (Session kvs) =
  let val = toQueryParam a
   in Session $ (k, val) : kvs


renderSession :: Session -> ByteString
renderSession (Session kvs) =
  cs $ T.intercalate "; " $ map keyPair kvs
 where
  keyPair (k, v) = k <> "=" <> v


-- parseSession :: ByteString -> Session
-- parseSession bs = Session $ mapMaybe keyValue $ parseQuery bs
--  where
--   keyValue :: QueryItem -> Maybe (Text, Text)
--   keyValue (_, Nothing) = Nothing
--   keyValue (k, Just v) = Just (cs k, cs v)

setSessionCookie :: Session -> (HeaderName, ByteString)
setSessionCookie sess = ("Set-Cookie", renderSession sess)


parseSessionCookies :: [(ByteString, ByteString)] -> Session
parseSessionCookies cks =
  Session $ map toText cks
 where
  toText (k, v) = (cs k, cs v)


sessionKey :: (FromHttpApiData a) => Text -> Session -> Either Text (Maybe a)
sessionKey k (Session kvs) =
  case L.lookup k kvs of
    Nothing -> pure Nothing
    Just t -> Just <$> parseQueryParam t
