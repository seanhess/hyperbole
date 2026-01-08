{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.Intro.BestPractices where

import App.Route
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
  = ViewFunctions
  deriving (Show, Enum, Bounded)
instance PageAnchor Basics

page :: (Hyperbole :> es) => Page es '[ViewFunctions.Message]
page = do
  pure $ layoutSubnav @Basics Basics $ do
    sectionA ViewFunctions $ do
      markdocs "We showed above how we can factor `View`s into functions. It's best practice to have a main `View` function for each `HyperView`. Create views as pure function of input data and state, by passing them in:"
      snippet "inputs -> View viewId ()"

      markdocs "We can write multiple view functions with our `HyperView` as the `context`, and factor them however is most convenient:"
      snippet $ raw $(embedTopLevel "Example.Docs.ViewFunctions" "^messageButton")

      markdocs "Some `View` functions can be used in any `context`:"
      snippet $ raw $(embedTopLevel "Example.Docs.ViewFunctions" "^header")

      markdocs "With those two functions defined, we can refactor our main `View` Function to use them and avoid repeating ourselves"
      snippet $ raw $(embedTopLevel "Example.Docs.ViewFunctions" "messageView")

      example ViewFunctions.source $ do
        hyper ViewFunctions.VFMessage $ ViewFunctions.messageView "Hello"
