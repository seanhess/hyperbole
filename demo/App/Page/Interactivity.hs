{-# LANGUAGE TemplateHaskell #-}

module App.Page.Interactivity where

import App.Docs
import App.Route hiding (Javascript)
import Example.Interactivity.Events
import Example.Interactivity.Inputs
import Example.Javascript as Javascript
import Example.View.Layout
import Web.Hyperbole

data Sections
  = Inputs
  | Events
  | Javascript
  deriving (Show, Bounded, Enum, PageAnchor)

page :: (Hyperbole :> es) => Page es '[Boxes, JBoxes, Message, TryEvents, Dropper]
page = do
  pure $ layoutSubnav @Sections Interactivity $ do
    -- NOTE: only include javascript on the pages you need it
    script "custom.js"

    section Inputs $ do
      markdocs $(embedFile "docs/interactivity-inputs.md")
      example $(moduleSourceNamed "Example.Interactivity.Inputs") $ hyper Dropper (selectPlanet Nothing)

    section Events $ do
      markdocs $(embedFile "docs/interactivity-events.md")
      example $(moduleSourceNamed "Example.Interactivity.Events") $ hyper TryEvents (viewEvents "")

      markdocs $(embedFile "docs/interactivity-events2.md")
      example $(moduleSourceNamed "Example.Interactivity.Events") $ hyper Boxes (viewBoxes Nothing)

    section Javascript $ do
      markdocs $(embedFile "docs/interactivity-javascript.md")
      example Javascript.source $ do
        hyper JBoxes $ viewJBoxes Nothing

      markdocs $(embedFile "docs/interactivity-pushevent.md")
      example Javascript.source $ do
        hyper Message viewMessage
