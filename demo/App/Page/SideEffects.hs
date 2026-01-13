{-# LANGUAGE TemplateHaskell #-}

module App.Page.SideEffects where

import App.Route as Route (AppRoute (SideEffects))
import App.Docs.Markdown
import App.Docs.Page
import Example.Counter (Counter (..))
import Example.Docs.SideEffects as SideEffects
import Example.View.Layout (layoutSubnav)
import Web.Hyperbole

data EffectsSection
  = Effectful
  | Other
  | Custom
  deriving (Show, Enum, Bounded)

instance PageAnchor EffectsSection where
  sectionTitle Other = "Reader and More"
  sectionTitle Custom = "Databases and Custom Effects"
  sectionTitle a = camelTitle a

page :: (Hyperbole :> es) => Page es '[Counter, SlowReader, Titler]
page = do
  pure $ layoutSubnav @EffectsSection Route.SideEffects $ do
    section Effectful $ do
      markdocs $(embedFile "docs/effectful.md")
      example SideEffects.source $ do
        hyper Titler titleView

    section Other $ do
      markdocs $(embedFile "docs/effects-other.md")
      example SideEffects.source $ do
        hyper SlowReader $ messageView "..."

    section Custom $ do
      markdocs $(embedFile "docs/effects-custom.md")
