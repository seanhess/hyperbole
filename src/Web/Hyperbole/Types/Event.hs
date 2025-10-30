module Web.Hyperbole.Types.Event where

import Data.Aeson (ToJSON)
import Data.String.Conversions (cs)
import Web.Hyperbole.Data.Encoded


-- | Serialized ViewId
newtype TargetViewId = TargetViewId {encoded :: Encoded}
  deriving newtype (ToJSON, Ord, Eq)


instance Show TargetViewId where
  show (TargetViewId e) = "TargetViewId " <> cs (encodedToText e)


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action
