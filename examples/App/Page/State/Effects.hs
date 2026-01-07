{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.State.Effects where

import App.Route hiding (Counter)
import Data.Text (pack)
import Docs.Examples
import Docs.Page
import Effectful
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Example.Style.Cyber as Cyber (btn, dataFeature)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole as Hyperbole

page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar Int) :> es) => Page es '[Counter]
page = do
  n <- getCount
  pure $ do
    layout State $ do
      el "For all but the simplest cases, we will want to use some sort of Effect to manage our state"
      el $ do
        text "Pages and update functions can run side effects before rendering. Here we add a "
        code "Reader (TVar Int)"
        text "to track the count. "
        text "Notice that the current count now persists after a browser refresh"
      el "Instead of a TVar, you might use a database, or some other external effect"
      example source $ do
        col ~ embed $ hyper Counter (viewCount n)

data Counter = Counter
  deriving (Generic, ViewId)

instance (Reader (TVar Int) :> es, Concurrent :> es) => HyperView Counter es where
  data Action Counter
    = Increment
    | Decrement
    deriving (Generic, ViewAction)

  update Increment = do
    n <- modifyCount (+ 1)
    pure $ viewCount n
  update Decrement = do
    n <- modifyCount (subtract 1)
    pure $ viewCount n

viewCount :: Int -> View Counter ()
viewCount n = row $ do
  col ~ gap 10 $ do
    el ~ dataFeature $ text $ pack $ show n
    row ~ gap 10 $ do
      button Decrement "Decrement" ~ btn
      button Increment "Increment" ~ btn

modifyCount :: (Concurrent :> es, Reader (TVar Int) :> es) => (Int -> Int) -> Eff es Int
modifyCount f = do
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
  liveApp quickStartDocument (runReader var . runConcurrent $ runPage page)

source :: ExampleSource
source = $(exampleSource)
