{-# LANGUAGE TemplateHaskell #-}

module App.Page.ViewFunctions where

import App.Route qualified as Route
import Docs
import Example.Docs.ViewFunctions as VF
import Example.View.Layout (layoutSubnav)
import Web.Hyperbole
import Web.Atomic.CSS

data Basics
  = ViewFunctions
  | NotComponents
  deriving (Show, Enum, Bounded)
instance PageAnchor Basics

page :: (Hyperbole :> es) => Page es '[Message, Toggler, Progress]
page = do
  pure $ layoutSubnav @Basics Route.ViewFunctions $ do
    section ViewFunctions $ do
      markdocs $(embedFile "docs/view-functions.md")

      example VF.source $ do
        hyper VFMessage $ messageView "Hello"

    section NotComponents $ do
      markdocs $(embedFile "docs/view-components.md")

      example VF.source $ do
        hyper Toggler $ toggler False

      col ~ pad (T 20) . gap 10$ do
        markdocs $(embedFile "docs/view-functions-wrap.md")

      example VF.source $ do
        hyper Progress $ workingHard 0.1

      col ~ pad (T 20) . gap 10 $ do
        markdocs $(embedFile "docs/view-functions-end.md")
