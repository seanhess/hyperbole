module Example.Docs.UniqueViewId where

import Control.Monad (forM_)
import Data.Text (Text, pack)
import Example.Colors
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole

page :: Page es '[Item]
page = do
  itemIds <- loadDummyItemIds
  pure $ do
    row ~ gap 4 $ do
      forM_ itemIds $ \uid -> do
        hyper (Item uid) itemUnloaded

-- Item ----------------------------------------------------------------

type UniqueId = Int
data Item = Item UniqueId
  deriving (Generic, ViewId)

instance HyperView Item es where
  data Action Item = Load
    deriving (Generic, ViewAction)

  update Load = do
    Item uid <- viewId
    item <- loadDummyItem uid
    pure $ itemLoaded item

itemUnloaded :: View Item ()
itemUnloaded = do
  Item uid <- viewId
  button Load ~ btn $ text $ "Load " <> pack (show uid)

itemLoaded :: Text -> View Item ()
itemLoaded msg = do
  el ~ bg SecondaryLight . color White . pad 10 $ text msg

-- Fake Database ------------------------------------------------------

loadDummyItem :: Int -> Eff es Text
loadDummyItem n =
  pure $ items !! n
 where
  items = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

loadDummyItemIds :: Eff es [Int]
loadDummyItemIds = pure [0 .. 4]


