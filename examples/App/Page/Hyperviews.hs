{-# LANGUAGE TemplateHaskell #-}

module App.Page.Hyperviews where

import App.Route qualified as Route (AppRoute (..))
import Docs
import Example.Counter (Counter (..))
import Example.Docs.MultiView qualified as Multi
import Example.Docs.Nesting qualified as Nesting
import Example.Docs.UniqueViewId qualified as UniqueViewId
import Example.Simple (Message (..))
import Example.Trigger as Trigger
import Example.View.Layout (layoutSubnav)
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types (Root (..))
import Web.Hyperbole.Page (subPage)

data HyperSectuions
  = IndependentUpdates
  | UniqueViewid
  | Nesting
  | TargetingOtherHyperviews
  deriving (Show, Enum, Bounded, PageAnchor)

page :: (Hyperbole :> es) => Page es '[Counter, Message, UniqueViewId.Item, Nesting.ItemList, Targeted, Controls]
page = do
  mlt <- subPage Multi.page
  uvd <- subPage UniqueViewId.page
  nst <- subPage Nesting.page
  pure $ layoutSubnav @HyperSectuions Route.Hyperviews $ do
    section IndependentUpdates $ do
      markdocs $(embedFile "docs/hyperviews-multi.md")
      example $(moduleSourceNamed "Example.Docs.MultiView") $ do
        runViewContext Root () mlt

    section UniqueViewid $ do
      markdocs $(embedFile "docs/hyperviews-unique.md")
      example $(moduleSourceNamed "Example.Docs.UniqueViewId") $ do
        runViewContext Root () uvd

    section Nesting $ do
      markdocs $(embedFile "docs/hyperviews-nesting.md")

      example $(moduleSourceNamed "Example.Docs.Nesting") $ do
        runViewContext Root () nst

    section TargetingOtherHyperviews $ do
      markdocs "Sometimes nesting isn't enough, and we need to directly communicate to other `HyperView`s. Below, we have an independent `HyperView` which displays a message, and two ways to control it:"
      example Trigger.source $ do
        hyper Targeted $ targetedView "..."

      markdocs "Use `trigger` to tell another `HyperView` to run an action"
      snippet $ do
        raw $(embedTopLevel "Example.Trigger" "instance HyperView Controls")

      example Trigger.source $ do
        hyper Controls controlView

      markdocs "Alternatively, you can use `target` in a `View` to use `Action`s from another `HyperView`"
      snippet $ do
        raw $(embedTopLevel "Example.Trigger" "targetView")

      example Trigger.source $ do
        hyper Controls targetView
