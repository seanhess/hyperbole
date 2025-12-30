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
data Event id act st = Event
  { viewId :: id
  , action :: act
  , state :: st
  }


instance (Show act, Show id, Show st) => Show (Event id act st) where
  show e = "Event " <> show e.viewId <> " " <> show e.action <> " " <> show e.state
