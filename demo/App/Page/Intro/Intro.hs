{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.Intro.Intro where

import App.Docs
import App.Route
import Data.String.Interpolate (i)
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
        example $(moduleSourceNamed "Example.Simple") $ do
          runViewContext Root () simple

        snippet $ do
          raw $(embedTopLevel "Example.Simple" "{-# LANGUAGE")
          raw "\nmodule Main where\n\n"
          raw $(embedSource "Example.Simple" (isTopLevel "import") (const True))

        section' "But Why?" $ do
          markdocs $(embedFile "docs/intro.md")
