module Web.Hyperbole.Types.Event where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Effectful
import Web.Hyperbole.HyperView (HyperView (..))
import Web.Hyperbole.HyperView.Types (decodeAction, decodeViewId)


-- | An id and an action, along with a unique id
data Event id act = Event
  { viewId :: id
  , action :: act
  , eventId :: RequestId
  }
  deriving (Show)


newtype EventId = EventId Text
  deriving (Show)


newtype RequestId = RequestId Text
  deriving newtype (ToJSON)
  deriving (Show)


-- must be in an effect to get `es` on the right-hand-side
decodeHyperEvent :: (HyperView id es) => Event TargetViewId Text -> Eff es (Maybe (Event id (Action id)))
decodeHyperEvent (Event (TargetViewId ti) ta eventId) = pure $ do
  vid <- decodeViewId ti
  act <- decodeAction ta
  pure $ Event vid act eventId


-- | Serialized ViewId
newtype TargetViewId = TargetViewId {text :: Text}
  deriving newtype (ToJSON)
  deriving (Show)
