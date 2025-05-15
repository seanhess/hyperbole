module Example.Page.CSS.External where

import Data.Text (Text)
import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage page)

page :: (Hyperbole :> es) => Eff es (Page '[Items])
page = do
  pure $ do
    -- you can choose to include a stylesheet only on the page that uses it
    -- or load it in your document function
    stylesheet "external.css"
    hyper Items $ itemsView "one"

data Items = Items
  deriving (Generic, ViewId)

instance HyperView Items es where
  data Action Items = Select Text
    deriving (Generic, ViewAction)

  update (Select t) = do
    pure $ itemsView t

itemsView :: Text -> View Items ()
itemsView sel = do
  el @ class_ "parent" $ do
    item "one"
    item "two"
    item "three"
    item "four"
    item "five"
 where
  selected i =
    if sel == i
      then class_ "selected"
      else id

  item i =
    -- the class_ attribute MERGES classes if you set it more than once
    button (Select i) @ class_ "item" . selected i $ text i
