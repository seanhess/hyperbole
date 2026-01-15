{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.CSS where

import App.Docs
import App.Route (AppRoute (CSS))
import Effectful
import Example.CSS.External qualified as External
import Example.CSS.Tooltips as Tooltips
import Example.CSS.Transitions as Transitions
import Example.Docs.CSS qualified as CSS
import Example.Interactivity.Events as Events
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types (Root (..))
import Web.Hyperbole.Page (subPage)

data CSSExample
  = Factoring
  | Transitions
  | Tooltips
  | External
  deriving (Eq, Generic, Show, Enum, Bounded)
instance PageAnchor CSSExample where
  sectionTitle = \case
    Factoring -> "Atomic CSS"
    Transitions -> "CSS Transitions"
    Tooltips -> "Tooltips"
    External -> "External Stylesheets"

page :: (Hyperbole :> es) => Page es '[Animate, External.Items, Boxes]
page = do
  ext <- subPage External.page
  pure $ layoutSubnav @CSSExample CSS $ do
    section Factoring $ do
      markdocs $(embedFile "docs/atomic.md")

      CSS.example ~ embed

    section Transitions $ do
      markdocs "If an update changes the `transition` property of a view, it will automatically animate with CSS Transitions, avoiding having the server compute animation frames."
      snippet $ do
        raw $(embedTopLevel "Example.CSS.Transitions" "viewSmall")
        raw "\n"
        raw $(embedTopLevel "Example.CSS.Transitions" "viewBig")
      example Transitions.source $ hyper Animate viewSmall

    section Tooltips $ do
      markdocs "For immediate feedback, create interactivity via Atomic CSS whenever possible."
      example Tooltips.source tooltips

    section External $ do
      markdocs "You can opt-out of Atomic CSS and use external classes with `class_`"
      snippet $ do
        raw $(embedTopLevel "Example.CSS.External" "page")
      snippet $ do
        raw $(embedTopLevel "Example.CSS.External" "itemsView")
      example External.source $ do
        runViewContext Root () ext
