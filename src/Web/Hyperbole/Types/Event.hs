module Web.Hyperbole.Types.Event where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.List qualified as L
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (Query, QueryItem)
import Web.Hyperbole.Data.Encoded (Encoded, encodedParseText)


-- | Serialized ViewId
newtype TargetViewId = TargetViewId {text :: Text}
  deriving newtype (ToJSON)


instance Show TargetViewId where
  show (TargetViewId t) = "TargetViewId " <> cs t


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action


-- lookupEvent :: Query -> Maybe (Event TargetViewId Encoded)

-- lookupEvent :: Query -> Maybe (Event TargetViewId Encoded)
-- lookupEvent q = do
--   viewId <- TargetViewId <$> lookupParamQueryString "hyp-id" q
--   actionText <- lookupParamQueryString "hyp-action" q
--   action <- getAction actionText
--   pure $ Event{viewId, action}
--  where
--   getAction :: Text -> Maybe Encoded
--   getAction inp = do
--     case encodedParseText inp of
--       Left _ -> Nothing
--       Right a -> pure a

-- | Lower-level lookup straight from the request
lookupParamQueryString :: ByteString -> Query -> Maybe Text
lookupParamQueryString key q = do
  mval <- L.lookup key q
  val <- mval
  pure $ cs val


isSystemParam :: QueryItem -> Bool
isSystemParam ("hyp-id", _) = True
isSystemParam ("hyp-action", _) = True
isSystemParam _ = False


queryRemoveSystem :: Query -> Query
queryRemoveSystem = filter (not . isSystemParam)
