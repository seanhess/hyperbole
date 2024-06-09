module Example.Concurrent where

import Data.Text (pack)
import Effectful
import Example.Effects.Debug
import System.Random (randomRIO)
import Web.Hyperbole


-- this is already running in a different context
page :: (Hyperbole :> es, Debug :> es, IOE :> es) => Page es Response
page = do
  handle content
  handle content2

  load $ do
    pure $ do
      row (pad 20) $ do
        col (gap 10 . border 1 . pad 20) $ do
          hyper Contents viewInit
        col (gap 10 . border 1 . pad 20) $ do
          hyper Contents2 viewInit2


data Contents = Contents
  deriving (Generic, Param)
instance HyperView Contents where
  type Action Contents = ContentsAction


data ContentsAction
  = Load
  | Reload Int
  deriving (Generic, Param)


data Contents2 = Contents2
  deriving (Generic, Param)
instance HyperView Contents2 where
  type Action Contents2 = ContentsAction2


data ContentsAction2
  = Load2
  | Reload2 Int
  deriving (Generic, Param)


content :: (Hyperbole :> es, Debug :> es, IOE :> es) => Contents -> ContentsAction -> Eff es (View Contents ())
content _ Load = do
  -- Pretend the initial Load takes 1s to complete
  liftIO (randomRIO (50, 150)) >>= delay
  pure $ onLoad (Reload 1) 100 $ el (color (HexColor "00ff00")) $ do
    el id "Loaded, should reload once more..."
content _ (Reload n) = do
  liftIO (randomRIO (50, 150)) >>= delay
  -- then reload after a 1s delay (client-side)
  pure $ onLoad (Reload (n + 1)) 100 $ do
    col (gap 10) $ el (color (HexColor "00ff00")) $ do
      el_ "Reloaded! polling..."
      el_ $ text $ pack $ show n


content2 :: (Hyperbole :> es, Debug :> es, IOE :> es) => Contents2 -> ContentsAction2 -> Eff es (View Contents2 ())
content2 _ Load2 = do
  -- Pretend the initial Load takes 1s to complete
  liftIO (randomRIO (50, 550)) >>= delay
  pure $ onLoad (Reload2 10000) 100 $ el (color (HexColor "ff0000")) $ do
    el id "Loaded, should reload once more..."
content2 _ (Reload2 n) = do
  liftIO (randomRIO (50, 550)) >>= delay
  -- then reload after a 1s delay (client-side)
  pure $ onLoad (Reload2 (n + 1)) 100 $ do
    col (gap 10) $ el (color (HexColor "ff0000")) $ do
      el_ "Reloaded! polling..."
      el_ $ text $ pack $ show n
      button (Reload2 (n + 1000)) id "Add"


viewInit :: View Contents ()
viewInit = do
  onLoad Load 0 $ do
    el (color (HexColor "00ff00")) "Lazy Loading..."


viewInit2 :: View Contents2 ()
viewInit2 = do
  onLoad Load2 0 $ do
    el (color (HexColor "ff0000")) "Lazy Loading..."
