{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Page.State.Actions where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Example.AppRoute
import Example.Page.Counter (Counter)
import Example.Page.Counter qualified as Counter
import Example.Style.Cyber (font)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView.Types

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  counter <- Counter.page
  pure $ exampleLayout (State Actions) $ do
    example' (routeTitle (State Actions)) (routeSource Counter) $ do
      hackage "#g:state" "Managing State"
      col ~ embed $ do
        pre countExample ~ font
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
