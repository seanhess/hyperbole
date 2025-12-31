module Example.Page.Forms where

import Docs.Page
import Example.AppRoute
import Example.Page.FormSimple (AddContact (..))
import Example.Page.FormSimple qualified as FormSimple
import Example.Page.FormValidation (Signup (..))
import Example.Page.FormValidation qualified as FormValidation
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[Signup, AddContact]
page = do
  pure $ layout (Forms FormSimple) $ do
    example (Forms FormSimple) $ do
      el "We can render and parse forms via a record"
      col ~ embed $ do
        hyper AddContact FormSimple.formView'

    example (Forms FormValidation) $ do
      el "Create validators for each field and render errors inline"
      col ~ embed $ do
        hyper Signup $ FormValidation.formView genFields
