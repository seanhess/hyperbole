{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Page.State.Actions where

import Example.AppRoute
import Example.Page.Counter (Counter)
import Data.String.Interpolate (i)
import Example.Page.Counter qualified as Counter
import Data.Text (Text)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  counter <- Counter.page
  pure $ exampleLayout (State Actions) $ do
    example "Action Context" "Example/Page/Counter.hs" $ do
      el "As shown in the intro, for very simple state, we can include it as a parameter on the Action"
      col ~ embed $ do
        pre countExample
      el "The state is only recorded in the rendered html, so if the user refreshes it will reset"
      col ~ embed $ do
        addContext Root counter

countExample :: Text
countExample = 
  [i|data Action Counter
  = Increment Int
  ...

  update (Increment n) = do
    pure $ viewCount (n + 1)

viewCount n = do
  button (Increment n) "Increment"|]
