module Example.Interactivity.Inputs where

import Data.Text (pack)
import Web.Atomic.CSS
import Web.Hyperbole hiding (button, input)

data Dropper = Dropper
  deriving (Generic, ViewId)

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  deriving (Generic, FromParam, ToParam, Eq, Show, Enum, Bounded)

instance HyperView Dropper es where
  data Action Dropper
    = Select (Maybe Planet)
    deriving (Generic, ViewAction)

  update (Select mp) = do
    pure $ selectPlanet mp

selectPlanet :: Maybe Planet -> View Dropper ()
selectPlanet mp = do
  dropdown Select mp ~ border 1 . pad 10 $ do
    option Nothing "Choose a Planet"
    option (Just Mercury) "Mercury"
    option (Just Venus) "Venus"
    option (Just Earth) "Earth"
    option (Just Mars) "Mars"
  case mp of
    Nothing -> none
    Just p -> el $ text $ "You chose: " <> pack (show p)
