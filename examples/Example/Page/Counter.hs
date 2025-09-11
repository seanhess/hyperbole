{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Counter where

import Data.Text (pack)
import Effectful
import Example.Style.Cyber as Style
import Web.Atomic.CSS
import Web.Hyperbole as Hyperbole

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  pure $ hyper Counter (viewCount 0)

data Counter = Counter
  deriving (Generic, ViewId)

instance HyperView Counter es where
  data Action Counter
    = Increment Int
    | Decrement Int
    deriving (Generic, ViewAction)

  update (Increment n) = do
    pure $ viewCount (n + 1)
  update (Decrement n) = do
    pure $ viewCount (n - 1)

viewCount :: Int -> View Counter ()
viewCount n = row $ do
  col ~ gap 10 $ do
    el ~ dataFeature $ text $ pack $ show n
    row ~ gap 10 $ do
      button (Decrement n) "Decrement" ~ Style.btn
      button (Increment n) "Increment" ~ Style.btn
