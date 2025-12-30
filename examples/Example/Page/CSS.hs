{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.Page.CSS where

import Data.Snippet
import Data.String.Interpolate (i)
import Effectful
import Example.AppRoute (AppRoute (CSS))
import Example.AppRoute qualified as Route
import Example.Page.CSS.External qualified as External
import Example.Page.CSS.Tooltips
import Example.Page.CSS.Transitions
import Example.Page.Interactivity.Events as Events
import Example.Style qualified as Style
import Example.View.Layout (embed, example, exampleLayout, section', snippet)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types (Root (..))
import Web.Hyperbole.Page (subPage)

page :: (Hyperbole :> es) => Page es '[Transitions, External.Items, Boxes]
page = do
  ext <- subPage External.page
  pure $ exampleLayout (CSS Route.CSSAll) $ do
    section' "Atomic CSS" $ do
      el $ do
        text "Hyperbole encourages using the "
        link [uri|https://github.com/seanhess/atomic-css|] ~ Style.link $ "Atomic CSS"
        text "package to factor styles with haskell functions"
      snippet $ do
        raw $(embedSource "examples/Example/Docs/CSS.hs" (isTopLevel "import") (const True))

      el $ do
        text "Note how we use "
        code "hover"
        text " to provide immediate feedback to the user without talking to the server"
      col ~ embed $ do
        el ~ h3 $ "My Page"
        el ~ border 1 . pad 10 . clickable $ "Hover Me"

    section' "CSS Transitions" $ do
      el "Animate changes with CSS Transitions"
      example (CSS Route.Transitions) $ hyper Transitions viewSmall

    section' "Tooltips" $ do
      el "For immediate feedback, create interactivity via Atomic CSS whenever possible"
      example (CSS Route.Tooltips) $ tooltips

    section' "External Stylesheets" $ do
      el $ do
        text "You can opt-out of Atomic CSS and use external classes with "
        code "class_"
      example (CSS Route.External) $ do
        addContext Root ext
 where
  header = bold
  h3 = header . fontSize 18
  clickable = pointer . hover bold
