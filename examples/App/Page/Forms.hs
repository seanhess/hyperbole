module App.Page.Forms where

import App.Route
import Docs.Page
import Example.FormSimple (AddContact (..))
import Example.FormSimple qualified as FormSimple
import Example.FormValidation (Signup (..))
import Example.FormValidation qualified as FormValidation
import Example.Style.Cyber (embed)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Signup, AddContact]
page = do
  pure $ layout (Forms FormSimple) $ do
    example FormSimple.source $ do
      el "We can render and parse forms via a record"
      col ~ embed $ do
        hyper AddContact FormSimple.formView'

    example FormValidation.source $ do
      el "Create validators for each field and render errors inline"
      col ~ embed $ do
        hyper Signup $ FormValidation.formView genFields
