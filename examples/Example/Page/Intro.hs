{-# LANGUAGE TemplateHaskell #-}

module Example.Page.Intro where

import Data.Snippet
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
    section' "Hello World" $ do
      el "Create interactive HTML applications with type-safe serverside Haskell. Inspired by HTMX, Elm, and Phoenix LiveView."

      example Simple ~ Cyber.font $ do
        runViewContext Root () simple

      snippet $ do
        raw $(embedSource "examples/Example/Page/Simple.hs" (const True) (const True))

    section' "Why Hyperbole?" $ do
      el "Single Page Applications (SPAs) require the programmer to write two programs: a Javascript client and a Server, which both must conform to a common API"
      el "Hyperbole allows us to instead write a single Haskell program which runs exclusively on the server. All user interactions are sent to the server for processing, and a sub-section of the page is updated with the resulting HTML."
      el "There are frameworks that support this in different ways, including HTMX, Phoenix LiveView, and others. Hyperbole has the following advantages"

    section' "Simple" $ do
      el "HyperViews update independently. In this example, two Message HyperViews are embedded into the same page with different ids."
      el "Try inspecting the page in the Chrome dev tools and watching both the DOM and messages"

    section Counter $ do
      el "Actions can have parameters for reusability, or to keep track of simple state"
      el $ do
        text "Use a view function to render the state: "
        code "viewCount :: Int -> View Counter ()."
        text "Notice how it expects the current count as a parameter."
      example Counter $ do
        runViewContext Root () counter
