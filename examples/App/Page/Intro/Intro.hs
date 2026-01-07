{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.Intro.Intro where

import App.Route
import Data.String.Interpolate (i)
import Docs.Examples
import Docs.Markdown
import Docs.Page
import Docs.Snippet
import Example.Colors
import Example.Counter (Counter)
import Example.Simple (Message)
import Example.Simple qualified as Simple
import Example.Style.Cyber qualified as Cyber
import Example.View.Layout (layout)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.Page (subPage)

-- import Example.Page.Counter qualified as Counter

page :: (Hyperbole :> es) => Page es '[Message, Counter]
page = do
  simple <- subPage Simple.page
  -- counter <- subPage Counter.page

  pure $ layout Intro $ do
    -- el ~ italic . fontSize 20 . pad (X 15) . border (X 3) . borderColor PrimaryLight $ "Create interactive HTML applications with type-safe serverside Haskell. Inspired by HTMX, Elm, and Phoenix LiveView"

    col ~ gap 20 $ do
      row ~ color cyan . bg Dark . pad 20 $ do
        space
        col ~ gap 10 $ do
          codeblock $ do
            [i|╔═════════════════════════════════════════════════════════════════════════════╗
║                                                                             ║
║  ██╗  ██╗██╗   ██╗██████╗ ███████╗██████╗ ██████╗  ██████╗ ██╗     ███████╗ ║
║  ██║  ██║╚██╗ ██╔╝██╔══██╗██╔════╝██╔══██╗██╔══██╗██╔═══██╗██║     ██╔════╝ ║
║  ███████║ ╚████╔╝ ██████╔╝█████╗  ██████╔╝██████╔╝██║   ██║██║     █████╗   ║
║  ██╔══██║  ╚██╔╝  ██╔═══╝ ██╔══╝  ██╔══██╗██╔══██╗██║   ██║██║     ██╔══╝   ║
║  ██║  ██║   ██║   ██║     ███████╗██║  ██║██████╔╝╚██████╔╝███████╗███████╗ ║
║  ╚═╝  ╚═╝   ╚═╝   ╚═╝     ╚══════╝╚═╝  ╚═╝╚═════╝  ╚═════╝ ╚══════╝╚══════╝ ║
╚═════════════════════════════════════════════════════════════════════════════╝
|]
          el ~ fontSize 18 . Cyber.font . bold . textAlign AlignCenter $ do
            el "Create interactive HTML applications with type-safe serverside Haskell."
            el "Inspired by HTMX, Elm, and Phoenix LiveView"
        space

      col ~ gap 10 $ do
        markdocs $(embedFile "docs/intro.md")

        section' "Simple Example" $ do
          example $(moduleSourceNamed "Example.Simple") $ do
            runViewContext Root () simple

        snippet $ do
          raw $(embedTopLevel "Example.Simple" "{-# LANGUAGE")
          raw "\nmodule Main where\n\n"
          raw $(embedSource "Example.Simple" (isTopLevel "import") (const True))

-- TODO: include counter?

-- markdocs "Using [atomic-css](https://hackage.haskell.org/package/atomic-css) we can use functions to factor styles as well"

-- section' "Simple" $ do
--   el "HyperViews update independently. In this example, two Message HyperViews are embedded into the same page with different ids."
--   el "Try inspecting the page in the Chrome dev tools and watching both the DOM and messages"
--
-- section Counter $ do
--   el "Actions can have parameters for reusability, or to keep track of simple state"
--   el $ do
--     text "Use a view function to render the state: "
--     code "viewCount :: Int -> View Counter ()."
--     text "Notice how it expects the current count as a parameter."
--   example Counter $ do
--     runViewContext Root () counter
