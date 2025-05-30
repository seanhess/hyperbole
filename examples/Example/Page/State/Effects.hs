{-# LANGUAGE UndecidableInstances #-}

module Example.Page.State.Effects where

import Data.Text (pack)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Example.AppRoute hiding (Counter)
import Example.Style as Style
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole as Hyperbole

page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar Int) :> es) => Eff es (Page '[Counter])
page = do
  n <- getCount
  pure $ do
    exampleLayout (State Effects) $ do
      example "Counter" "Example/Page/State/Effects.hs" $ do
        el $ do
          text "Pages and update functions can run side effects before rendering. Here we add a "
          code "Reader (TVar Int)"
          text "to track the count"
        el "Notice that the current count now persists after a browser refresh"
        col ~ embed $ hyper Counter (viewCount n)

data Counter = Counter
  deriving (Generic, ViewId)

instance (Reader (TVar Int) :> es, Concurrent :> es) => HyperView Counter es where
  data Action Counter
    = Increment
    | Decrement
    deriving (Generic, ViewAction)

  update Increment = do
    n <- modify (+ 1)
    pure $ viewCount n
  update Decrement = do
    n <- modify (subtract 1)
    pure $ viewCount n

viewCount :: Int -> View Counter ()
viewCount n = col ~ gap 10 $ do
  row $ do
    el ~ bold . fontSize 48 . border 1 . pad (XY 20 0) $ text $ pack $ show n
  row ~ gap 10 $ do
    button Decrement "Decrement" ~ Style.btn
    button Increment "Increment" ~ Style.btn

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
