module App.Page.Javascript where

import App.Route as Route
import Docs.Page
import Example.Javascript
import Example.Style.Cyber (btn, font)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Boxes, Message]
page = do
  pure $ layout Route.Javascript $ do
    -- NOTE: include custom javascript only on this page
    script "custom.js"

    el "Include custom js on a page with the script tag on only the page where it is needed, or globally via your toDocument function"

    section' "Javascript - runAction" $ do
      el $ do
        text "JS can call the server via an API attached to "
        code "window.Hyperbole"
        text ". Here we re-implement mouseover boxes from the Interactivity example using Javascript"

      example source ~ font $ do
        hyper Boxes $ viewBoxes Nothing

    section' "Javascript - pushEvent" $ do
      el "The server can push an event to be dispatched on a HyperView"

      example source $ do
        hyper Message $ button AlertMe ~ btn $ "Alert Me"
