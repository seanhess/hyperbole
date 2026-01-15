module App.Page.Examples where

import App.Docs
import App.Route as Route
import Example.Style as Style (link)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[]
page = do
  pure $ layout (Examples OtherExamples) $ do
    section' "UI Demos" $ do
      col ~ gap 10 $ do
        card (Examples Tags) "Add and remove \"tags\" from via an input"
        card (Examples Chat) "Demonstrates server pushes"
    -- card (Contacts ContactsAll) "Random "

    section' "Data Lists" $ do
      col ~ gap 10 $ do
        card (Data SortableTable) "Sort by column, demonstrates view functions"
        card (Data Autocomplete) "Incremental search using only hyperbole"
        card (Data Filter) "Faceted search, live filtering of lists  "
        card (Data LoadMore) "Progressively load more items"

    section' "Other Features" $ do
      card (Examples OAuth2) "Demonstration of OAuth2"

    section' "Reference Implementations" $ do
      card (Examples Todos) "using Atomic CSS"
      card (Examples TodosCSS) "using external classes"
 where
  card r cnt = row ~ gap 5 $ do
    route r ~ Style.link $ do
      text $ routeTitle r
    text "-"
    el $ text cnt

-- cardBtn :: (Styleable h) => CSS h -> CSS h
-- cardBtn =
--   bgAnimated
--     . bgGradient White
--     . hover bgzero
--     . clip 10
--     . shadow ()
--
-- grid :: (Styleable h) => CSS h -> CSS h
-- grid =
--   utility
--     "grid-ex"
--     [ "display" :. "grid"
--     , "grid-template-columns" :. "repeat(auto-fit, minmax(200px, 1fr))"
--     ]
--
-- tile :: (Styleable h) => CSS h -> CSS h
-- tile =
--   utility
--     "tile"
--     [ "aspect-ratio" :. "16 / 9"
--     ]

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
