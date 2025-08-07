module Example.Page.Interactivity where

import Example.AppRoute
import Example.Page.Interactivity.Events
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Eff es (Page '[Boxes])
page = do
  pure $ exampleLayout Interactivity $ do
    example "Events" "Example/Page/Interactivity/Events.hs" $ do
      el $ do
        text "Hyperbole provides various events that can be tied to specific actions. Up to this point, we've only used them via the higher-level views like "
        code "button"
        text " and "
        code "search"
        text ". They can also be used directly. In this example we use "
        code "onMouseEnter"
        text " and "
        code "onMouseLeave"
      el $ do
        text "Remember it is better to use Atomic CSS to provide immediate feedback whenever possible. If used improperly, too many mouse events could make the app unresponsive."
      col ~ embed $ hyper Boxes (viewBoxes Nothing)
