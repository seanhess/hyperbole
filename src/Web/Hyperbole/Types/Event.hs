module Web.Hyperbole.Types.Event where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Effectful


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


-- | Serialized ViewId
newtype TargetViewId = TargetViewId {text :: Text}
  deriving newtype (ToJSON)
  deriving (Show)
