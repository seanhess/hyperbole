module Example.Interactivity.Search where

import Data.Text (Text, pack)
import Effectful
import Example.Style.Cyber as Style
import Web.Atomic.CSS
import Web.Hyperbole as Hyperbole

page :: (Hyperbole :> es) => Page es '[CustomText]
page = do
  pure $ hyper CustomText (viewCustom "")

data CustomText = CustomText
  deriving (Generic, ViewId)

instance HyperView CustomText es where
  data Action CustomText
    = ShowInput Text
    deriving (Generic, ViewAction)

  update (ShowInput t) =
    pure $ viewCustom t

customText :: View CustomText ()
customText = do
  search ShowInput 250 ~ border 1 . pad 10 @ placeholder "Type something"

viewCustom :: Text -> View CustomText ()
viewCustom t = row $ do
  col ~ gap 10 $ do
    customText
    case t of
      "" -> none
      _ -> el $ text $ "You typed: " <> t
