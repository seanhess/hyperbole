module Example.Test where

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

-- TEST: add a test for Page+trigger
page :: (Hyperbole :> es, IOE :> es) => Page es '[Long]
page = do
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
