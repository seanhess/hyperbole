{-# LANGUAGE QuasiQuotes #-}

module Example.Page.CSS where

import Data.String.Interpolate (i)
import Effectful
import Example.AppRoute
import Example.Page.CSS.External qualified as External
import Example.Page.CSS.Tooltips
import Example.Page.CSS.Transitions
import Example.Page.Interactivity.Events as Events
import Example.Style qualified as Style
import Example.View.Layout (embed, example, exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types (Root (..))

page :: (Hyperbole :> es) => Eff es (Page '[Transitions, External.Items, Boxes])
page = do
  ext <- External.page
  pure $ exampleLayout CSS $ do
    example "Atomic CSS" "Example/Page/CSS.hs" $ do
      el $ do
        text "Hyperbole encourages using the "
        link [uri|https://github.com/seanhess/atomic-css|] ~ Style.link $ "Atomic CSS"
        text "package to factor styles with haskell functions"
      col ~ embed $ do
        pre
          [i|import Web.Atomic.CSS
import Web.Hyperbole

header = bold
h1 = header . fontSize 32
h2 = header . fontSize 24
h3 = header . fontSize 18
clickable = pointer . hover bold

example = do
  col $ do
    el ~ h3 $ "My Page"
    el ~ border 1 . pad 10 . clickable $ "Hover Me"
  ...
|]

      el $ do
        text "Note how we use "
        code "hover"
        text " to provide immediate feedback to the user without talking to the server"
      col ~ embed $ do
        el ~ h3 $ "My Page"
        el ~ border 1 . pad 10 . clickable $ "Hover Me"

    example "CSS Transitions" "Example/Page/CSS/Transitions.hs" $ do
      el "Animate changes with CSS Transitions"
      col ~ embed $ hyper Transitions viewSmall

    example "Tooltips" "Example/Page/CSS/Tooltips.hs" $ do
      el "For immediate feedback, create interactivity via Atomic CSS whenever possible"
      col ~ embed $ tooltips

    example "External Stylesheets" "Example/Page/CSS/External.hs" $ do
      el $ do
        text "You can opt-out of Atomic CSS and use external classes with "
        code "class_"
      addContext Root ext
 where
  header = bold
  h3 = header . fontSize 18
  clickable = pointer . hover bold
