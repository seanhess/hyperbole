module Example.Page.Forms where

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
  pure $ exampleLayout Forms $ do
    example "Simple Forms" "Example/Page/FormSimple.hs" $ do
      col ~ embed $ do
        hyper AddContact FormSimple.formView'

    example "Form Validation" "Example/Page/FormValidation.hs" $ do
      -- el $ do
      --   code "Validated"
      --   text " allows us to manage validation states for each form field"
      col ~ embed $ do
        hyper Signup $ FormValidation.formView genFields
