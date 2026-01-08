{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.State where

import App.Route (AppRoute (State))
import Docs.Markdown
import Docs.Page
import Docs.Snippet
import Example.Counter as Counter
import Example.View.Layout (layoutSubnav)
import Web.Hyperbole

data StateSection
  = Stateless
  | ActionThreading
  | ViewState
  deriving (Show, Enum, Bounded)

instance PageAnchor StateSection

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  pure $ layoutSubnav @StateSection State $ do
    sectionA Stateless stateless
    sectionA ActionThreading actions
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
