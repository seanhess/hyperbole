{-# LANGUAGE UndecidableInstances #-}

module Example.LazyLoading where

import Data.Text (pack)
import Effectful
import Example.AppRoute as Route
import Example.Effects.Debug
import Example.View.Layout (exampleLayout)
import Web.Hyperbole

page :: (Hyperbole :> es, Debug :> es) => Eff es (Page '[Contents])
page = do
  pure $ exampleLayout LazyLoading $ do
    row (pad 20) $ do
      col (gap 10 . border 1 . pad 20) $ do
        hyper Contents viewInit

data Contents = Contents
  deriving (Show, Read, ViewId)

instance (Debug :> es) => HyperView Contents es where
  data Action Contents
    = Load
    | Reload Int
    deriving (Show, Read, ViewAction)
  update Load = do
    -- Pretend the initial Load takes 1s to complete
    delay 1000
    pure $ do
      el (onLoad (Reload 1) 1000) "Loaded, should reload once more..."
  update (Reload n) = do
    -- then reload after a 1s delay (client-side)
    pure $ do
      col (gap 10 . onLoad (Reload (n + 1)) 1000) $ do
        el_ "Reloaded! polling..."
        el_ $ text $ pack $ show n

viewInit :: View Contents ()
viewInit = do
  el (onLoad Load 0) "Lazy Loading..."
