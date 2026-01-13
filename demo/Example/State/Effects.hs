{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.State.Effects where

import Data.Text (pack)
import App.Docs.Examples
import Effectful
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Example.Style.Cyber as Cyber (btn, dataFeature)
import Web.Atomic.CSS
import Web.Hyperbole as Hyperbole
import Web.Hyperbole.Data.Encoded

page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar Int) :> es) => Page es '[Counter]
page = do
  n <- getCount
  pure $ do
    hyper Counter (viewCount n)

data Counter = Counter
  deriving (Generic)
instance ViewId Counter where
  -- to avoid conflicts with other "Counter" ViewIds on example pages
  toViewId _ = Encoded "counter-effects" []
  parseViewId _ = pure Counter

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

source :: ModuleSource
source = $(moduleSource)
