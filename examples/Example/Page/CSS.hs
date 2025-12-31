{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.Page.CSS where

import Docs.Markdown
import Docs.Snippet
import Effectful
import Example.AppRoute (AppRoute (CSS))
import Example.AppRoute qualified as Route
import Example.Page.CSS.External qualified as External
import Example.Page.CSS.Tooltips
import Example.Page.CSS.Transitions as Transitions
import Example.Page.Interactivity.Events as Events
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types (Root (..))
import Web.Hyperbole.Page (subPage)

page :: (Hyperbole :> es) => Page es '[Transitions, External.Items, Boxes]
page = do
  ext <- subPage External.page
  pure $ exampleLayout CSS $ do
    section' "Atomic CSS" $ do
      markdocs "Hyperbole encourages using the [atomic-css](https://github.com/seanhess/atomic-css)  package to factor styles with haskell functions"

      snippet $ do
        raw $(embedSource "examples/Example/Docs/CSS.hs" (isTopLevel "import") (const True))

      markdocs "Note how we use `hover` to provide immediate feedback to the user without talking to the server"

      col ~ embed $ do
        el ~ h3 $ "My Page"
        el ~ border 1 . pad 10 . clickable $ "Hover Me"

    sectionA Route.Transitions $ do
      el "Animate changes with CSS Transitions"
      example Route.Transitions $ hyper Transitions viewSmall

    sectionA Route.Tooltips $ do
      el "For immediate feedback, create interactivity via Atomic CSS whenever possible"
      example Route.Tooltips tooltips

    sectionA Route.External $ do
      el $ do
        text "You can opt-out of Atomic CSS and use external classes with "
        code "class_"
      example Route.External $ do
        runViewContext Root () ext
 where
  header = bold
  h3 = header . fontSize 18
  clickable = pointer . hover bold
