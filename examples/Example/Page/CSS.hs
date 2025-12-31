{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.Page.CSS where

import Data.String.Conversions (cs)
import Docs.Markdown
import Docs.Page
import Docs.Snippet
import Effectful
import Example.AppRoute (AppRoute (CSS))
import Example.Docs.CSS qualified as CSS
import Example.Page.CSS.External qualified as External
import Example.Page.CSS.Tooltips
import Example.Page.CSS.Transitions as Transitions
import Example.Page.Interactivity.Events as Events
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
instance ExampleSource CSSExample where
  exampleSource c = ["Example", "Page", "Intro", "CSS", cs (show c) <> ".hs"]

page :: (Hyperbole :> es) => Page es '[Animate, External.Items, Boxes]
page = do
  ext <- subPage External.page
  pure $ layoutSubnav @CSSExample CSS $ do
    sectionA Factoring $ do
      markdocs "Hyperbole encourages using the [atomic-css](https://github.com/seanhess/atomic-css)  package to factor styles with haskell functions. In this example, we create reusable header and btn styles."

      snippet $ do
        raw $(embedSource "examples/Example/Docs/CSS.hs" (isTopLevel "import") (const True))

      markdocs "Note how we use `hover` to provide immediate feedback to the user without talking to the server"

      CSS.example ~ embed

    sectionA Transitions $ do
      el "If an update changes the `transition` property of a view, it will automatically animate with CSS Transitions, avoiding having the server compute animation frames."
      snippet $ do
        raw $(embedTopLevel "examples/Example/Page/CSS/Transitions.hs" "viewSmall")
        raw "\n"
        raw $(embedTopLevel "examples/Example/Page/CSS/Transitions.hs" "viewBig")
      example Transitions $ hyper Animate viewSmall

    sectionA Tooltips $ do
      el "For immediate feedback, create interactivity via Atomic CSS whenever possible."
      example Tooltips tooltips

    sectionA External $ do
      el $ do
        text "You can opt-out of Atomic CSS and use external classes with "
        code "class_"
      snippet $ do
        raw $(embedTopLevel "examples/Example/Page/CSS/External.hs" "page")
      snippet $ do
        raw $(embedTopLevel "examples/Example/Page/CSS/External.hs" "itemsView")
      example External $ do
        runViewContext Root () ext
