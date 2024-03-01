module Example.LazyLoading where

import Effectful
import Example.Effects.Debug
import Web.Hyperbole


-- this is already running in a different context
page :: (Hyperbole :> es, Debug :> es) => Page es Response
page = do
  hyper content

  load $ do
    pure $ do
      row (pad 20) $ do
        col (gap 10 . border 1 . pad 20) $ do
          viewId Contents viewInit


data Contents = Contents
  deriving (Show, Read, Param)
instance HyperView Contents where
  type Action Contents = ContentsAction


data ContentsAction
  = Load
  | Reload
  deriving (Show, Read, Param)


content :: (Hyperbole :> es, Debug :> es) => Contents -> ContentsAction -> Eff es (View Contents ())
content _ Load = do
  -- go really slow!
  delay 1000

  -- then reload after a 1s delay (client-side)
  pure $ onLoad Reload 1000 $ do
    el id "Loaded, should reload once more..."
content _ Reload = do
  pure $ col (gap 10) $ do
    el_ "Reloaded!"


viewInit :: View Contents ()
viewInit = do
  onLoad Load 0 $ do
    el id "Lazy Loading..."
