{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Page.Intro where

import Example.AppRoute
import Example.Page.Counter (Counter)
import Example.Page.Counter qualified as Counter
import Example.Page.Simple (Message)
import Example.Page.Simple qualified as Simple
import Example.Style.Cyber as Cyber (font)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.Page (subPage)

page :: (Hyperbole :> es) => Page es '[Message, Counter]
page = do
  simple <- subPage Simple.page
  counter <- subPage Counter.page

  pure $ exampleLayout Intro $ do
    section Simple $ do
      el "HyperViews update independently. In this example, two Message HyperViews are embedded into the same page with different ids."
      el "Try inspecting the page in the Chrome dev tools and watching both the DOM and messages"
      example Simple ~ Cyber.font $ do
        addContext Root simple

    section Counter $ do
      el "Actions can have parameters for reusability, or to keep track of simple state"
      el $ do
        text "Use a view function to render the state: "
        code "viewCount :: Int -> View Counter ()."
        text "Notice how it expects the current count as a parameter."
      example Counter $ do
        addContext Root counter
