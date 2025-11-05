{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Page.State.Actions where

import Data.Snippet
import Data.String.Interpolate (i)
import Data.Text (Text)
import Example.AppRoute
import Example.Page.Counter (Counter)
import Example.Page.Counter qualified as Counter
import Example.View.Layout
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  counter <- Counter.page
  pure $ exampleLayout (State Actions) $ do
    section (State Actions) $ do
      hackage "#g:state" "Managing State"

      snippet $ do
        raw $(embedTopLevel "examples/Example/Page/Counter.hs" "instance HyperView")

      snippet $ do
        raw $(embedTopLevel "examples/Example/Page/Counter.hs" "viewCount")

      example Counter $ do
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
