{-# LANGUAGE TemplateHaskell #-}

module App.Page.Forms where

import App.Docs
import App.Route
import Example.FormSimple (AddContact (..))
import Example.FormSimple qualified as FormSimple
import Example.FormValidation (Signup (..))
import Example.FormValidation qualified as FormValidation
import Example.View.Layout
import Web.Hyperbole

data Sections
  = BasicForms
  | Validation
  deriving (Generic, Show, Bounded, Enum, PageAnchor)

page :: (Hyperbole :> es) => Page es '[Signup, AddContact]
page = do
  pure $ layoutSubnav @Sections (Forms FormSimple) $ do
    section BasicForms $ do
      markdocs $(embedFile "docs/forms-simple.md")

      example FormSimple.source $ do
        hyper AddContact FormSimple.formView'

    section Validation $ do
      markdocs $(embedFile "docs/forms-validated.md")

      example FormValidation.source $ do
        --
        hyper Signup $ FormValidation.formView genFields
