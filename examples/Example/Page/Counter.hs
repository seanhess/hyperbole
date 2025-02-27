{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Counter where

import Data.Text (pack)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Example.AppRoute qualified as Route
import Example.Style as Style
import Example.View.Layout (exampleLayout)
import Web.Hyperbole as Hyperbole

page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar Int) :> es) => Eff es (Page '[Counter])
page = do
  n <- getCount
  pure $ exampleLayout Route.Counter $ do
    col (pad 20 . gap 10) $ do
      hyper Counter (viewCount n)

data Counter = Counter
  deriving (Show, Read, ViewId)

instance (Reader (TVar Int) :> es, Concurrent :> es) => HyperView Counter es where
  data Action Counter
    = Increment
    | Decrement
    deriving (Show, Read, ViewAction)

  update Increment = do
    n <- modify (+ 1)
    pure $ viewCount n
  update Decrement = do
    n <- modify (subtract 1)
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

getCount :: (Concurrent :> es, Reader (TVar Int) :> es) => Eff es Int
getCount = readTVarIO =<< ask

initCounter :: (Concurrent :> es) => Eff es (TVar Int)
initCounter = newTVarIO 0

app :: TVar Int -> Application
app var = do
  liveApp (basicDocument "Example") (runReader var . runConcurrent $ runPage page)
