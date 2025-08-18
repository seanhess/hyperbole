module Web.Hyperbole.Types.Event where

import Data.Aeson (ToJSON)
import Data.String.Conversions (cs)
import Data.Text (Text)


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
