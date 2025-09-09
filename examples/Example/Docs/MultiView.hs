{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.MultiView where

import Data.Text (pack)
import Example.Docs.Interactive (Message (..), messageView)
import Web.Atomic.CSS
import Web.Hyperbole

page :: Page es [Message, Count]
page = do
  pure $ do
    hyper Message $ messageView "Hello"
    hyper Count $ countView 0

data Count = Count
  deriving (Generic, ViewId)

instance HyperView Count es where
  data Action Count
    = Increment Int
    | Decrement Int
    deriving (Generic, ViewAction)

  update (Increment n) = do
    pure $ countView (n + 1)
  update (Decrement n) = do
    pure $ countView (n - 1)

countView :: Int -> View Count ()
countView n = do
  el $ text $ pack $ show n
  button (Increment n) "Increment" ~ border 1
  button (Decrement n) "Decrement" ~ border 1
