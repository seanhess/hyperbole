{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.Intro.Basics where

import App.Route
import Data.String.Interpolate (i)
import Docs.Markdown
import Docs.Page
import Docs.Snippet
import Example.Counter (Counter)
import Example.Docs.Interactive as Interactive
import Example.Docs.ViewFunctions qualified as ViewFunctions
import Example.View.Layout (layoutSubnav)
import Web.Atomic.CSS
import Web.Hyperbole

data Basics
  = GetRunning
  | HtmlViews
  | Interactive
  | ViewFunctions
  deriving (Show, Enum, Bounded)

-- TODO: move to basics
instance PageAnchor Basics where
  sectionTitle Interactive = "Interactive HyperViews"
  sectionTitle HtmlViews = "HTML Views"
  sectionTitle GetRunning = "Get Running"
  sectionTitle ViewFunctions = "View Functions"

  navEntry Interactive = "HyperViews"
  navEntry a = sectionTitle a

page :: (Hyperbole :> es) => Page es '[Message, Counter, ViewFunctions.Message]
page = do
  pure $ layoutSubnav @Basics Basics $ do
    sectionA GetRunning getRunning
    sectionA HtmlViews htmlViews
    sectionA Interactive interactive
    sectionA ViewFunctions viewFunctions
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

  interactive = do
    markdocs "We can embed one or more `HyperView`s to add type-safe interactivity to live subsections of our `Page`. To start, first define a data type (a `ViewId`) that uniquely identifies that subsection of the page:"
    snippet $ raw $ $(embedTopLevel "Example.Docs.Interactive" "data Message")
    markdocs
      [i|Make our `ViewId` an instance of `HyperView`:

  * Create an `Action` type with a constructor for every possible way that the user can interact with it
  * Write an `update` for each `Action`|]
    snippet $ raw $ $(embedTopLevel "Example.Docs.Interactive" "instance HyperView Message")
    markdocs
      [i|If an `Action` occurs, the contents of our `HyperView` will be replaced with the result of `update`.

  To use our new `HyperView`, add the `ViewId` to the type-level list of `Page`, and then place it in the page view with `hyper`.|]

    snippet $ raw $(embedTopLevel "Example.Docs.Interactive" "page")

    markdocs "Now let's add a button to trigger the `Action`. Note that instead of using a generic `context` in our `View` type signature, we must set it to our `ViewId`. The compiler will tell us if we try to trigger actions that don't belong to our `HyperView`"
    snippet $ raw $(embedTopLevel "Example.Docs.Interactive" "messageView")

    markdocs "If the user clicks the button, the contents of `hyper` will be replaced with the result of `update`, leaving the rest of the page untouched."

    example Interactive.source $ do
      hyper Message $ messageView "Hello World"

  viewFunctions = do
    markdocs "We showed above how we can factor `View`s into functions. It's best practice to have a main `View` function for each `HyperView`. Create views as pure function of input data and state, by passing them in"
    snippet "inputs -> View viewId ()"

    markdocs "We can write multiple view functions with our `HyperView` as the `context`, and factor them however is most convenient:"
    snippet $ raw $(embedTopLevel "Example.Docs.ViewFunctions" "^messageButton")

    markdocs "Some `View` functions can be used in any `context`:"
    snippet $ raw $(embedTopLevel "Example.Docs.ViewFunctions" "^header")

    markdocs "With those two functions defined, we can refactor our main `View` Function to use them and avoid repeating ourselves"
    snippet $ raw $(embedTopLevel "Example.Docs.ViewFunctions" "messageView")

    example ViewFunctions.source $ do
      hyper ViewFunctions.VFMessage $ ViewFunctions.messageView "Hello"
