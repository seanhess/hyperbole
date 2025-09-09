module Example.Page.Javascript where

import Data.Text (Text, pack)
import Example.AppRoute as Route
import Example.Page.Interactivity.Events (box, viewBoxes')
import Example.Style.Cyber (btn, font)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Boxes, Message]
page = do
  pure $ exampleLayout Route.Javascript $ do
    -- NOTE: include custom javascript only on this page
    script "custom.js"

    el "Include custom js on a page with the script tag on only the page where it is needed, or globally via your toDocument function"

    example' "Javascript - runAction" (routeSource Javascript) $ do
      el $ do
        text "JS can call the server via an API attached to "
        code "window.Hyperbole"
        text ". Here we re-implement mouseover boxes from the Interactivity example using Javascript"

      col ~ embed . font $ do
        hyper Boxes $ viewBoxes Nothing

    example' "Javascript - pushEvent" (routeSource Javascript) $ do
      el "The server can push an event to be dispatched on a HyperView"

      col ~ embed $ do
        hyper Message $ button AlertMe ~ btn $ "Alert Me"

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

data Message = Message
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message = AlertMe
    deriving (Generic, ViewAction)

  update AlertMe = do
    pushEvent @Text "server-message" "hello"
    pure "Sent 'server-message' event"
