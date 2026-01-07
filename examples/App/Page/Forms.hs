module App.Page.Forms where

import App.Route
import Docs.Page
import Example.FormSimple (AddContact (..))
import Example.FormSimple qualified as FormSimple
import Example.FormValidation (Signup (..))
import Example.FormValidation qualified as FormValidation
import Example.View.Layout
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Signup, AddContact]
page = do
  pure $ layout (Forms FormSimple) $ do
    section' "Basic Forms" $ do
      el "We can render and parse forms via a record"
      example FormSimple.source $ do
        hyper AddContact FormSimple.formView'

    section' "Validated Forms" $ do
      el "Create validators for each field and render errors inline"
      example FormValidation.source $ do
        hyper Signup $ FormValidation.formView genFields
