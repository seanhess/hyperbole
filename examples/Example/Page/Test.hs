{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Test where

import Control.Exception
import Control.Monad (forM_, when)
import Data.String.Conversions (cs)
import Effectful
import Example.AppRoute
import Example.Effects.Debug
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

-- TEST: add a test for Page+trigger
page :: (Hyperbole :> es, Debug :> es, IOE :> es) => Page es '[Long]
page = do
  -- trigger Fake Noop
  pure $ exampleLayout Test $ do
    -- script "test.js"
    example Test $ do
      col ~ embed $ do
        hyper Long longView

data Long = Long
  deriving (Generic, ViewId)

instance (Debug :> es, IOE :> es) => HyperView Long es where
  data Action Long
    = Start
    | Interrupt
    deriving (Generic, ViewAction)

  type Concurrency Long = Replace

  update Start = do
    forM_ [0 .. 10] $ \n -> do
      pushUpdate $ activeView n
      when (n > 5) $
        liftIO $ do
          throwIO $ MyException "OH NO"
      delay 1000
    pure $ do
      el "All Done"
      button Interrupt ~ border 1 $ "Ok Go Now"
  update Interrupt = do
    -- the problem is, while you can have it do other things, the other request is still running!
    -- so you really don't want to execute a request while one is currently happening...
    -- per-user lock?
    pure "Interrupted"

data MyException = MyException String
  deriving (Show, Exception)

longView :: View Long ()
longView = do
  col ~ gap 10 $ do
    el "LONG"
    button Start ~ border 1 $ "woo"

activeView :: Int -> View Long ()
activeView n = do
  col ~ gap 10 $ do
    el "RUNNING"
    el $ text $ cs $ show n
    button Interrupt ~ border 1 $ "Interrupt"

-- data Message = Message1 | Message2
--   deriving (Generic, ViewId, Show)
--
-- instance (Debug :> es) => HyperView Message es where
--   data Action Message
--     = Louder Text
--     deriving (Generic, ViewAction)
--
--   update (Louder m) = do
--     v :: Message <- viewId
--     traceM $ "Updating " <> show v
--     let new = m <> "!"
--     delay 1000
--     traceM $ " - done " <> show v
--     pure $ messageView new
--
-- messageView :: Text -> View Message ()
-- messageView m = do
--   row ~ gap 10 $ do
--     button (Louder m) ~ border 1 . pad 5 . whenLoading (borderColor Danger . color Danger) $ "Louder"
--     el ~ pad 5 $ text m
--
-- data Other = Other
--   deriving (Generic, ViewId)
--
-- instance (Debug :> es) => HyperView Other es where
--   type Require Other = '[Message]
--
--   data Action Other
--     = GoTrigger
--     | Sneaky
--     deriving (Generic, ViewAction)
--
--   update GoTrigger = do
--     -- trigger Fake Noop
--     trigger Message2 (Louder "remote")
--     pushEvent "hello" (String "woot")
--     pure "OK"
--   update Sneaky = do
--     pure "Sneaky"
--
-- ---- NOT ON PAGE ---------------------------
--
-- data Fake = Fake
--   deriving (Generic, ViewId)
--
-- instance HyperView Fake es where
--   data Action Fake
--     = Noop
--     deriving (Generic, ViewAction)
--
--   update _ = do
--     pure "OK"
