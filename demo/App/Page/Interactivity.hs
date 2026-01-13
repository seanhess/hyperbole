{-# LANGUAGE TemplateHaskell #-}

module App.Page.Interactivity where

import App.Route hiding (Javascript)
import App.Docs
import Example.Interactivity.Events
import Example.Javascript as Javascript
import Example.View.Layout
import Web.Hyperbole

data Sections
  = Events
  | Javascript
  deriving (Show, Bounded, Enum, PageAnchor)

page :: (Hyperbole :> es) => Page es '[Boxes, JBoxes, Message]
page = do
  pure $ layoutSubnav @Sections Interactivity $ do
    -- NOTE: only include javascript on the pages you need it
    script "custom.js"

    section Events $ do
      markdocs $(embedFile "docs/interactivity-events.md")
      example $(moduleSourceNamed "Example.Interactivity.Events") $ hyper Boxes (viewBoxes Nothing)

    section Javascript $ do
      markdocs $(embedFile "docs/interactivity-javascript.md")

      example Javascript.source $ do
        hyper JBoxes $ viewJBoxes Nothing

      markdocs $(embedFile "docs/interactivity-pushevent.md")

      example Javascript.source $ do
        hyper Message viewMessage
