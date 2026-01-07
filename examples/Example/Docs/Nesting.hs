module Example.Docs.Nesting where

import Control.Monad (forM_)
import Example.Colors
import Example.Docs.UniqueViewId hiding (loadDummyItemIds)
import Example.Style.Cyber (btnLight)
import Web.Atomic.CSS
import Web.Hyperbole

page :: Page es '[ItemList, Item]
page = do
  itemIds <- loadDummyItemIds
  pure $ hyper ItemList $ itemList itemIds

data ItemList = ItemList
  deriving (Generic, ViewId)

instance HyperView ItemList es where
  data Action ItemList = Reset
    deriving (Generic, ViewAction)

  type Require ItemList = '[Item]

  update Reset = do
    itemIds <- loadDummyItemIds
    pure $ itemList itemIds

-- need to load different item ids, because both examples are on the same documentation page!
loadDummyItemIds :: Eff es [Int]
loadDummyItemIds = pure [5 .. 9]

itemList :: [Int] -> View ItemList ()
itemList itemIds = do
  row ~ gap 4 . color White $ do
    forM_ itemIds $ \itemId -> do
      hyper (Item itemId) itemUnloaded
    button Reset ~ btnLight $ "Reset"
