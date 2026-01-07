{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.SideEffects where

import App.Route as Route (AppRoute (SideEffects))
import Docs.Markdown
import Docs.Page
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

-- example SideEffects.source $ do
--   hyper SlowReader $ messageView ""

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

-- -- The `Hyperbole` effect gives us access to the request and client state, including sessions and the query params.
-- viewState = do
--   snippet $ raw "HI"
