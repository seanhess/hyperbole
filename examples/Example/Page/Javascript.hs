module Example.Page.Javascript where

import Data.Text (pack)
import Example.AppRoute qualified as Route
import Example.Page.Interactivity.Events (box, viewBoxes')
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Eff es (Page '[Boxes])
page = do
  pure $ exampleLayout Route.Javascript $ do
    -- NOTE: include custom javascript only on this page
    script "custom.js"

    example "JS Events" "Example/Page/Javascript.hs" $ do
      el "Include custom js on a page with the 'script' tag only on the page where it is needed"
      el "You can call the server from Javascript via an API attached to window. Here we re-implement mouseover boxes from the Interactivity example using Javascript"

      col ~ embed $ do
        hyper Boxes $ viewBoxes Nothing

data Boxes = Boxes
  deriving (Generic, ViewId)

instance HyperView Boxes es where
  data Action Boxes
    = Selected Int
    | Clear
    deriving (Generic, ViewAction)

  update (Selected n) = do
    pure $ viewBoxes (Just n)
  update Clear = do
    pure $ viewBoxes Nothing

viewBoxes :: Maybe Int -> View Boxes ()
viewBoxes mn = do
  viewBoxes' mn $ \n -> do
    el ~ box . cls "box" $ text $ pack $ show n
