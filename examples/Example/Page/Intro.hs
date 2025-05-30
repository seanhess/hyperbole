{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Page.Intro where

import Example.AppRoute
import Example.Page.Counter (Counter)
import Example.Page.Counter qualified as Counter
import Example.Page.Simple (Message)
import Example.Page.Simple qualified as Simple
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types

page :: (Hyperbole :> es) => Eff es (Page '[Message, Counter])
page = do
  simple <- Simple.page
  counter <- Counter.page
  pure $ exampleLayout Intro $ do
    example "Simple" "Example/Page/Simple.hs" $ do
      el "HyperViews update independently. In this example, two Message HyperViews are embedded into the same page with different ids."
      el "Try inspecting the page in the Chrome dev tools and watching both the DOM and messages"
      col ~ embed $ do
        addContext Root simple

    example "Counter" "Example/Page/Counter.hs" $ do
      el "Actions can have parameters for reusability, or to keep track of simple state"
      el $ do
        text "Use a view function to render the state: "
        code "viewCount :: Int -> View Counter ()."
        text "Notice how it expects the current count as a parameter"
      col ~ embed $ do
        addContext Root counter
