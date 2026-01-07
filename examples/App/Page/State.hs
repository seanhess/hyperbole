{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.State where

import App.Route (AppRoute (State))
import Data.String.Interpolate (i)
import Docs.Markdown
import Docs.Page
import Docs.Snippet
import Example.Counter as Counter
import Example.Docs.Interactive
import Example.Docs.ViewFunctions qualified as ViewFunctions
import Example.View.Layout (layoutSubnav)
import Web.Atomic.CSS
import Web.Hyperbole

data StateSection
  = Stateless
  | ActionThreading
  | SideEffects
  | ViewState
  deriving (Show, Enum, Bounded)

instance PageAnchor StateSection

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  pure $ layoutSubnav @StateSection State $ do
    sectionA Stateless stateless
    sectionA ActionThreading actions
    sectionA SideEffects sideEffects
    sectionA ViewState viewState
 where
  stateless = do
    markdocs "By default, `HyperView`s are stateless. No state is stored in the server connection. `HyperView` `update`s are the direct result of processing the `Action`."
    snippet $
      raw $(embedSource "Example.State.Stateless" (isTopLevel "data Swap") (const True))

  actions = do
    markdocs "The simplest way to add state is to pass it back and forth between the `Action` and the `View`. Here's an implementation of the classic counter example. Notice how each action expects an `Int`, which represents the current count:"
    snippet $
      raw
        $(embedTopLevel "Example.Counter" "instance HyperView Counter")

    markdocs "We also pass the current count to our `View` Function. Then each button includes it in its corresponding action:"
    snippet $
      raw
        $(embedTopLevel "Example.Counter" "viewCount")

    example Counter.source $ do
      hyper Counter $ viewCount 0

  sideEffects = do
    markdocs "For any real application with more complex state and data persistence, we need side effects."
    markdocs "Hyperbole relies on [Effectful](https://hackage.haskell.org/package/effectful) to compose side effects. We can use these `Effect`s in a `Page` or an `update`. In this example the page keeps the message in the query params."
    snippet $ raw "HI"

  -- The `Hyperbole` effect gives us access to the request and client state, including sessions and the query params.
  viewState = do
    snippet $ raw "HI"
