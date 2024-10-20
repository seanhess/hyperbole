module Example.Counter where

import Data.Text (pack)
import Effectful
import Effectful.Concurrent.STM
import Example.Style as Style
import Web.Hyperbole


-- We are using a TVar to manage our state
-- In normal web applications, state will be managed in a database, abstracted behind a custom Effect. See Example.Effects.Users for the interface
-- Optionally, the count could be stored in a session. See Example.Sessions
page :: (Hyperbole :> es, Concurrent :> es) => TVar Int -> Page es '[Counter]
page var = do
  handle (counter var) $ load $ do
    n <- readTVarIO var
    pure $ col (pad 20 . gap 10) $ do
      el h1 "Counter"
      hyper Counter (viewCount n)


data Counter = Counter
  deriving (Show, Read, ViewId)
instance HyperView Counter where
  type Action Counter = Count


data Count
  = Increment
  | Decrement
  deriving (Show, Read, ViewAction)


counter :: (Hyperbole :> es, Concurrent :> es) => TVar Int -> Counter -> Count -> Eff es (View Counter ())
counter var _ Increment = do
  n <- modify var $ \n -> n + 1
  pure $ viewCount n
counter var _ Decrement = do
  n <- modify var $ \n -> n - 1
  pure $ viewCount n


viewCount :: Int -> View Counter ()
viewCount n = col (gap 10) $ do
  row id $ do
    el (bold . fontSize 48 . border 1 . pad (XY 20 0)) $ text $ pack $ show n
  row (gap 10) $ do
    button Decrement Style.btn "Decrement"
    button Increment Style.btn "Increment"


modify :: (Concurrent :> es) => TVar Int -> (Int -> Int) -> Eff es Int
modify var f =
  atomically $ do
    modifyTVar var f
    readTVar var


initCounter :: (Concurrent :> es) => Eff es (TVar Int)
initCounter = newTVarIO 0
