{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.Page.Intro.Basics where

import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Docs.Markdown
import Docs.Page
import Docs.Snippet
import Example.AppRoute
import Example.Page.Counter (Counter)
import Example.Page.Simple (Message)
import Example.View.Layout (layoutSubnav)
import Web.Atomic.CSS
import Web.Hyperbole

data Basics
  = Pages
  | HtmlViews
  | Interactive
  deriving (Show, Enum, Bounded)

-- TODO: move to basics
instance PageAnchor Basics where
  sectionTitle Interactive = "Interactive HyperViews"
  sectionTitle HtmlViews = "HTML Views"
  sectionTitle b = cs (show b)

  navEntry Interactive = "HyperViews"
  navEntry a = sectionTitle a

page :: (Hyperbole :> es) => Page es '[Message, Counter]
page = do
  pure $ layoutSubnav @Basics Basics $ do
    sectionA Pages $ do
      markdocs $(embedFile "docs/getting-started.md")
      snippet $ do
        raw $ $(embedTopLevel "examples/Example/Docs/BasicPage.hs" "main")
        raw "\n"
        raw $ $(embedTopLevel "examples/Example/Docs/BasicPage.hs" "page")
      col ~ embed $ do
        "Hello World"

    sectionA HtmlViews $ do
      markdocs "`View`s are HTML fragments with a `context`"
      snippet $ raw $ $(embedTopLevel "examples/Example/Docs/BasicPage.hs" "helloWorld")
      snippet $
        text
          [i|>>> renderText helloWorld
<div>Hello World</div>|]
      markdocs "We can factor `View`s into reusable functions:"
      snippet $ do
        rawMulti
          [ $(embedTopLevel "examples/Example/Docs/BasicPage.hs" "messageView")
          , $(embedTopLevel "examples/Example/Docs/BasicPage.hs" "page'")
          ]
      col ~ embed $ do
        "Hello World"
      markdocs "Using [atomic-css](/css) we can use functions to factor styles as well"

    sectionA Interactive $ do
      markdocs "We can embed one or more `HyperView`s to add type-safe interactivity to live subsections of our `Page`. To start, first define a data type (a `ViewId`) that uniquely identifies that subsection of the page:"
      snippet $ raw $ $(embedTopLevel "examples/Example/Docs/Interactive.hs" "data Message")
      markdocs
        [i|Make our `ViewId` an instance of `HyperView`:

* Create an `Action` type with a constructor for every possible way that the user can interact with it
* Write an `update` for each `Action`|]
      snippet $ raw $ $(embedTopLevel "examples/Example/Docs/Interactive.hs" "instance HyperView Message")
      markdocs
        [i|If an `Action` occurs, the contents of our `HyperView` will be replaced with the result of `update`.
To use our new `HyperView`, add the `ViewId` to the type-level list of `Page`, and then place it in the page view with `hyper`.|]
