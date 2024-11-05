{-# LANGUAGE UndecidableInstances #-}

module Example.Counter where

import Data.Text (pack)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Example.Style as Style
import Web.Hyperbole as Hyperbole


page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar Int) :> es) => Page es '[Counter]
page = load $ do
  var <- ask
  n <- readTVarIO var
  pure $ col (pad 20 . gap 10) $ do
    el h1 "Counter"
    hyper Counter (viewCount n)


data Counter = Counter
  deriving (Show, Read, ViewId)


data Count
  = Increment
  | Decrement
  deriving (Show, Read, ViewAction)


instance HyperView Counter where
  type Action Counter = Count
instance (Reader (TVar Int) :> es, Concurrent :> es) => Handle Counter es where
  handle _ Increment = do
    n <- modify $ \n -> n + 1
    pure $ viewCount n
  handle _ Decrement = do
    n <- modify $ \n -> n - 1
    pure $ viewCount n


viewCount :: Int -> View Counter ()
viewCount n = col (gap 10) $ do
  row id $ do
    el (bold . fontSize 48 . border 1 . pad (XY 20 0)) $ text $ pack $ show n
  row (gap 10) $ do
    button Decrement Style.btn "Decrement"
    button Increment Style.btn "Increment"


modify :: (Concurrent :> es, Reader (TVar Int) :> es) => (Int -> Int) -> Eff es Int
modify f = do
  var <- ask
  atomically $ do
    modifyTVar var f
    readTVar var


initCounter :: (Concurrent :> es) => Eff es (TVar Int)
initCounter = newTVarIO 0
