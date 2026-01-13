{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.Application where

import App.Docs
import App.Route (AppRoute (Application))
import Effectful
import Example.CSS.External qualified as External
import Example.CSS.Transitions as Transitions
import Example.Interactivity.Events as Events
import Example.View.Layout
import Web.Hyperbole

data Sections
  = LiveApp
  | Document
  | Pages
  | TypeSafeRoutes
  | RunningEffects
  deriving (Eq, Generic, Show, Enum, Bounded, PageAnchor)

page :: (Hyperbole :> es) => Page es '[Animate, External.Items, Boxes]
page = do
  pure $ layoutSubnav @Sections Application $ do
    section LiveApp $ do
      markdocs $(embedFile "docs/app-live.md")

    section Document $ do
      markdocs $(embedFile "docs/app-document.md")

    section Pages $ do
      markdocs $(embedFile "docs/app-pages.md")

    section TypeSafeRoutes $ do
      markdocs $(embedFile "docs/app-routes.md")

    section RunningEffects $ do
      markdocs $(embedFile "docs/app-effects.md")
