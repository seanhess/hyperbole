{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Page.State.View where

import App.Route qualified as Route
import Data.Text (pack)
import Docs.Examples
import Docs.Page
import Example.Style.Cyber as Style
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  let rt = Route.State
  pure $ layout rt $ do
    example $(exampleSource) $ do
      col ~ embed $ do
        hyperState Counter 1 viewCount

data Counter = Counter
  deriving (Generic)
instance ViewId Counter where
  type ViewState Counter = Int

instance HyperView Counter es where
  data Action Counter
    = Increment
    | Decrement
    deriving (Generic, ViewAction)

  update Increment = do
    modify @Int (+ 1)
    pure viewCount
  update Decrement = do
    modify @Int (subtract 1)
    pure viewCount

viewCount :: View Counter ()
viewCount = row $ do
  n <- viewState
  col ~ gap 10 $ do
    el ~ dataFeature $ text $ pack $ show n
    row ~ gap 10 $ do
      button Decrement "Decrement" ~ Style.btn
      button Increment "Increment" ~ Style.btn
