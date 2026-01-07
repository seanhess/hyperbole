{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Page.State.Actions where

import App.Route
import Data.String.Interpolate (i)
import Data.Text (Text)
import Docs.Page
import Docs.Snippet
import Example.Counter (Counter)
import Example.Counter qualified as Counter
import Example.View.Layout
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  counter <- Counter.page
  pure $ layout State $ do
    section State $ do
      -- hackage "#g:state" "Managing State"

      snippet $ do
        raw $(embedTopLevel "Example.Counter" "instance HyperView")

      snippet $ do
        raw $(embedTopLevel "Example.Counter" "viewCount")

      example Counter.source $ do
        runViewContext Root () counter

countExample :: Text
countExample =
  [i|data Action Counter
  = Increment Int
  ...

  update (Increment n) = do
    pure $ viewCount (n + 1)

viewCount n = do
  button (Increment n) "Increment"|]
