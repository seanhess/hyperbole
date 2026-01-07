{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.SideEffects where

import App.Route as Route (AppRoute (SideEffects))
import Data.String.Interpolate (i)
import Docs.Markdown
import Docs.Page
import Docs.Snippet

-- import Example.Docs.Interactive

import Example.Counter (Counter (..), viewCount)
import Example.Docs.SideEffects as SideEffects
import Example.View.Layout (layoutSubnav)
import Web.Atomic.CSS
import Web.Hyperbole

data EffectsSection
  = Effectful
  deriving (Show, Enum, Bounded)

instance PageAnchor EffectsSection

page :: (Hyperbole :> es) => Page es '[Counter, SlowMessage]
page = do
  pure $ layoutSubnav @EffectsSection Route.SideEffects $ do
    sectionA Effectful sideEffects
 where
  -- stateless = do
  --   markdocs "By default, `HyperView`s are stateless. No state is stored in the server connection. `HyperView` `update`s are the direct result of processing the `Action`."
  --   snippet $
  --     raw $(embedSource "examples/Example/Page/State/Stateless.hs" (isTopLevel "data Swap") (const True))
  --
  -- actions = do
  --   markdocs "The simplest way to add state is to pass it back and forth between the `Action` and the `View`. Here's an implementation of the classic counter example. Notice how each action expects an `Int`, which represents the current count:"
  --   snippet $
  --     raw
  --       $(embedTopLevel "examples/Example/Page/Counter.hs" "instance HyperView Counter")
  --
  --   markdocs "We also pass the current count to our `View` Function. Then each button includes it in its corresponding action:"
  --   snippet $
  --     raw
  --       $(embedTopLevel "examples/Example/Page/Counter.hs" "viewCount")
  --
  --   example ActionThreading $ do
  --     hyper Counter $ viewCount 0

  sideEffects = do
    markdocs "Hyperbole relies heavily on [Effectful](https://hackage.haskell.org/package/effectful) to run and compose side effects. We can use these `Effect`s in any `Page` or `update`."
    markdocs "The `Hyperbole` effect is automatically available in both, and gives us direct access to the client connection. We can use it to get information about the `request`, update the page directly, and more. Here is how you might use it to set the page title:"
    snippet $ do
      raw $(embedTopLevel "Example.Docs.SideEffects" "instance HyperView Titler")

    markdocs "If we want to use an `Effect` besides `Hyperbole`, add it as a constraint to the `page` and `update`. This `HyperView` uses `Concurrent` to delay every response by 500ms:"
    snippet $ do
      raw "{-# LANGUAGE UndecidableInstances #-}\n\n"
      raw $(embedTopLevel "Example.Docs.SideEffects" "data SlowMessage")
      raw "\n"
      raw $(embedTopLevel "Example.Docs.SideEffects" "instance (Concurrent :> es) => HyperView SlowMessage")

    example SideEffects.source $ do
      hyper SlowMessage $ messageView ""

-- -- The `Hyperbole` effect gives us access to the request and client state, including sessions and the query params.
-- viewState = do
--   snippet $ raw "HI"
