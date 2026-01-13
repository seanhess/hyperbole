{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.Intro.Basics where

import App.Route
import Data.String.Interpolate (i)
import App.Docs
import Example.Counter (Counter)
import Example.Docs.Interactive qualified as Interactive
import Example.Docs.ViewFunctions qualified as ViewFunctions
import Example.Simple as Simple
import Example.View.Layout (layoutSubnav)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types (Root (..))
import Web.Hyperbole.Page (subPage)

data Basics
  = GetRunning
  | HtmlViews
  | Interactive
  deriving (Show, Enum, Bounded)

instance PageAnchor Basics where
  sectionTitle Interactive = "Interactive HyperViews"
  sectionTitle a = camelTitle a

  navEntry Interactive = "HyperViews"
  navEntry a = sectionTitle a

page :: (Hyperbole :> es) => Page es '[Message, Counter, ViewFunctions.Message]
page = do
  int <- subPage Interactive.page
  -- mlt <- subPage Multi.page
  pure $ layoutSubnav @Basics Basics $ do
    section GetRunning getRunning
    section HtmlViews htmlViews

    -- section Styles $ do
    --   markdocs $(embedFile "docs/atomic.md")
    --   CSS.example ~ embed
    --   markdocs "See [Styles](/css) for more info"

    section Interactive $ do
      markdocs $(embedFile "docs/hyperviews-intro.md")

      example $(moduleSourceNamed "Example.Simple") $ do
        runViewContext Root () int
 where
  getRunning = do
    markdocs "Hyperbole applications are divided into top-level `Page`s, which run side effects, then return an HTML `View`"
    snippet $ raw $ $(embedTopLevel "Example.Docs.BasicPage" "hello")

    markdocs "Run an Application via [Warp](https://hackage.haskell.org/package/warp) and [WAI](https://hackage.haskell.org/package/wai). This runs on port 3000 and responds to everything with \"Hello World\""

    snippet $ do
      raw $ $(embedTopLevel "Example.Docs.BasicPage" "main")

    col ~ embed $ do
      "Hello World"

  htmlViews = do
    markdocs "`View`s are HTML fragments with a `context`"
    snippet $ raw $ $(embedTopLevel "Example.Docs.BasicPage" "helloWorld")

    --  WARNING: this doesn't render properly when embedded in markdown
    snippet $
      text
        [i|>>> renderText helloWorld
"<div>Hello World</div>"|]

    markdocs "We can factor `View`s into reusable functions:"
    snippet $ do
      rawMulti
        [ $(embedTopLevel "Example.Docs.BasicPage" "messageView")
        , $(embedTopLevel "Example.Docs.BasicPage" "page")
        ]

    col ~ embed $ do
      "Hello World"

    markdocs "Using [atomic-css](/css) we can use functions to factor styles as well"
