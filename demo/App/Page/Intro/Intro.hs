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

page :: (Hyperbole :> es) => Page es '[Message, Counter]
page = do
  simple <- subPage Simple.page

  pure $ layout Intro $ do
    col ~ gap 20 $ do
      row ~ color cyan . bg Dark . pad 20 $ do
        space
        col ~ gap 10 . overflow Hidden $ do
          row $ do
            space
            codeblock ~ scaleText $ do
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
            space
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

        section' "When not to use Hyperbole?" $ do
          markdocs $(embedFile "docs/intro-downsides.md")

        section' "Documentation" $ do
          markdocs $(embedFile "docs/intro-links.md")
 where
  scaleText :: (Styleable h) => CSS h -> CSS h
  scaleText =
    utility
      "scale-text"
      [ "font-size" :. "clamp(0.4rem, 1.5vw, 1rem)"
      , "max-width" :. "100%"
      ]
