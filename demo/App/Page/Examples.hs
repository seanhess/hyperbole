module App.Page.Examples where

import App.Route as Route
import App.Docs
import Example.Colors
import Example.Style.Cyber
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[]
page = do
  pure $ layout (Examples OtherExamples) $ do
    section' "Other Examples" $ do
      el ~ grid . gap 10 $ do
        card (Examples Tags) "Tag component"
        card (Examples Todos) "Implementation of TodoMVC using Atomic CSS"
        card (Examples TodosCSS) "Implementation of TodoMVC using external classes"
        card (Examples OAuth2) "Demonstration of OAuth2"

    section' "Data Lists" $ do
      el ~ grid . gap 10 $ do
        card (Data SortableTable) "Tag component"
        card (Data Autocomplete) "Tag component"
        card (Data Filter) "Tag component"
        card (Data LoadMore) "testing asdf"
 where
  card r cnt = route r ~ cardBtn . tile $ do
    el ~ bold . fontSize 20 . color White . bg PrimaryLight . font . pad 4 . textAlign AlignCenter $ do
      text $ routeTitle r
    el ~ pad 10 $ text cnt

  cardBtn :: (Styleable h) => CSS h -> CSS h
  cardBtn =
    bgAnimated
      . bgGradient White
      . hover bgzero
      . clip 10
      . shadow ()

  grid :: (Styleable h) => CSS h -> CSS h
  grid =
    utility
      "grid-ex"
      [ "display" :. "grid"
      , "grid-template-columns" :. "repeat(auto-fit, minmax(200px, 1fr))"
      ]

  tile :: (Styleable h) => CSS h -> CSS h
  tile =
    utility
      "tile"
      [ "aspect-ratio" :. "16 / 9"
      ]

-- section Effectful $ do
--   markdocs $(embedFile "docs/effectful.md")
--   example SideEffects.source $ do
--     hyper Titler titleView
--
-- section Other $ do
--   markdocs $(embedFile "docs/effects-other.md")
--   example SideEffects.source $ do
--     hyper SlowReader $ messageView "..."
--
-- section Custom $ do
--   markdocs $(embedFile "docs/effects-custom.md")
