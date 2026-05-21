module Example.Scrollbars where

import Control.Monad (forM_)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Example.Colors
import Example.Style.Cyber (btn, btnLight)
import Web.Atomic.CSS
import Web.Hyperbole

test :: IO ()
test = do
  putStrLn "Starting..."
  run 3000 $ do
    liveApp quickStartDocument (runPage page)

page' :: (Hyperbole :> es) => Page es '[Long]
page' = do
  pure $ do
    style "body { height: 100vh; overflow: hidden; } "
    hyper Long (longView Nothing) ~ height (Pct 1)

data Long = Long
  deriving (Generic, ViewId)

instance HyperView Long es where
  data Action Long
    = Select Text
    deriving (Generic, ViewAction)

  update (Select t) = do
    pure $ longView (Just t)

longView :: Maybe Text -> View Long ()
longView sel = do
  row ~ height (Pct 1) $ do
    col ~ gap 10 . pad 10 . bg cyan . width 200 . height (Pct 1) . overflow Auto $ do
      forM_ [0 .. 100 :: Int] $ \n -> do
        let val = cs $ "Item " <> show n
        button (Select val) ~ btnLight . slide val $ text val

    col ~ gap 10 . pad 20 . border 3 . grow $ do
      el ~ bold $ "SELECTED"
      case sel of
        Nothing -> "_"
        Just t -> el $ text t
 where
  slide v =
    if Just v == sel
      then color White . bold . btn
      else btnLight

data Test = Test deriving (Generic, ViewId)

instance HyperView Test es where
  data Action Test = Noop
    deriving (Generic, ViewAction)

  update Noop = do
    pure none

page :: Page es '[Test]
page = pure $ do
  el ~ vh100 . overflow Hidden $ do
    col ~ height (Pct 1) . pad 25 . gap 30 $ do
      hyper Test ~ height (Pct 1) $ do
        col ~ overflow Scroll . height 300 . width 300 . border 1 $ do
          forM_ [0 .. 100 :: Int] $ \_ -> do
            el "HELLO"
 where
  vh100 = utility "vh100" ["height" :. "100vh"]
