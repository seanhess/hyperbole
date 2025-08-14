module Web.Hyperbole.Types.Event where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.List qualified as L
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (Query, QueryItem)


-- | Serialized ViewId
newtype TargetViewId = TargetViewId {text :: Text}
  deriving (Show)
  deriving newtype (ToJSON)


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action


lookupEvent :: Query -> Maybe (Event TargetViewId Text)
lookupEvent q = do
  viewId <- TargetViewId <$> lookupParamQueryString "hyp-id" q
  action <- lookupParamQueryString "hyp-action" q
  pure $ Event{viewId, action}


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
