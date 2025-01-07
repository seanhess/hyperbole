{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.MultiView where

import Data.Text (pack)
import Example.Docs.Interactive (Message (..), messageView)
import Web.Hyperbole

page :: Eff es (Page [Message, Count])
page = do
  pure $ do
    row id $ do
      hyper Message $ messageView "Hello"
      hyper Count $ countView 0

data Count = Count
  deriving (Show, Read, ViewId)

instance HyperView Count es where
  data Action Count
    = Increment Int
    | Decrement Int
    deriving (Show, Read, ViewAction)

  update (Increment n) = do
    pure $ countView (n + 1)
  update (Decrement n) = do
    pure $ countView (n - 1)

countView :: Int -> View Count ()
countView n = do
  el_ $ text $ pack $ show n
  button (Increment n) (border 1) "Increment"
  button (Decrement n) (border 1) "Decrement"
