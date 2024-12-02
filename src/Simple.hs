{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Simple where

import Data.Text (pack)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Reader.Dynamic
import Web.Hyperbole


main = do
  count <- runEff $ runConcurrent initCounter
  run 3000 $ do
    liveApp (basicDocument "Example") (response count)


response :: (Hyperbole :> es, Concurrent :> es) => TVar Int -> Eff es Response
response count = runReader count $ runPage page


page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar Int) :> es) => Page es '[Counter]
page = do
  var <- ask
  n <- readTVarIO var
  pure $ col (pad 20 . gap 10) $ do
    el id "Counter"
    hyper Counter (viewCount n)


data Counter = Counter
  deriving (Show, Read, ViewId)


instance (Reader (TVar Int) :> es, Concurrent :> es) => HyperView Counter es where
  data Action Counter
    = Increment
    | Decrement
    deriving (Show, Read, ViewAction)


  handle Increment = do
    Counter <- viewId
    n <- modify $ \n -> n + 1
    pure $ viewCount n
  handle Decrement = do
    n <- modify $ \n -> n - 1
    pure $ viewCount n


viewCount :: Int -> View Counter ()
viewCount n = col (gap 10) $ do
  row id $ do
    el (bold . fontSize 48 . border 1 . pad (XY 20 0)) $ text $ pack $ show n
  row (gap 10) $ do
    button Decrement id "Decrement"
    button Increment id "Increment"


modify :: (Concurrent :> es, Reader (TVar Int) :> es) => (Int -> Int) -> Eff es Int
modify f = do
  var <- ask
  atomically $ do
    modifyTVar var f
    readTVar var


initCounter :: (Concurrent :> es) => Eff es (TVar Int)
initCounter = newTVarIO 0

-- simplePage :: (Hyperbole :> es, IOE :> es) => Eff es (Page '[MainView, Status])
-- simplePage = do
--   liftIO $ putStrLn "MAIN LOAD"
--   pure $ col (pad 20) $ do
--     el bold "My Page"
--     hyper MainView $ do
--       row (gap 10) $ do
--         button GoBegin (border 1) "Start"
--
--
-- simplePage1 :: (Hyperbole :> es, IOE :> es) => Eff es (Page '[Floop])
-- simplePage1 = do
--   liftIO $ putStrLn "MAIN LOAD"
--   pure $ col (pad 20) $ do
--     el bold "My Page"
--     hyper Floop $ do
--       row (gap 10) $ do
--         button FloopA (border 1) "Start"
--
--
-- simplePage0 :: (Hyperbole :> es, IOE :> es) => Eff es (Page '[])
-- simplePage0 = do
--   liftIO $ putStrLn "MAIN LOAD"
--   pure $ col (pad 20) $ do
--     el bold "My Page"
--
--
-- data Floop = Floop
--   deriving (Show, Read, ViewId)
--
--
-- data FloopA = FloopA
--   deriving (Show, Read, ViewAction)
--
--
-- instance HyperView Floop where
--   type Action Floop = FloopA
-- instance Handle Floop es where
--   handle _ _ = pure none
--
--
-- -- MAIN ----------------------------------------
--
-- data MainView = MainView
--   deriving (Show, Read, ViewId)
--
--
-- data MainAction
--   = GoBegin
--   | GoMid
--   | GoEnd
--   deriving (Show, Read, ViewAction)
--
--
-- instance HyperView MainView where
--   type Action MainView = MainAction
--   type Require MainView = '[Status]
-- instance Handle MainView es where
--   handle _ = \case
--     GoBegin -> pure beginStep
--     GoMid -> pure middleStep
--     GoEnd -> pure endStep
--
--
-- beginStep :: View MainView ()
-- beginStep = do
--   el_ "BEGIN"
--   button GoMid (border 1) " Mid"
--
--
-- middleStep :: View MainView ()
-- middleStep = do
--   el_ "MIDDLE: running"
--   button GoBegin (border 1) "Back"
--   hyper Status $ statusView 0
--
--
-- endStep :: View MainView ()
-- endStep = do
--   el_ "END"
--   button GoMid (border 1) "Back"
--
--
-- -- Status ---------------------------------------
--
-- data Status = Status
--   deriving (Show, Read, ViewId)
--
--
-- data CheckStatus
--   = CheckStatus Int
--   deriving (Show, Read, ViewAction)
--
--
-- instance HyperView Status where
--   type Action Status = CheckStatus
-- instance Handle Status es where
--   handle _ = \case
--     CheckStatus n ->
--       if n >= 5
--         then pure lazyEnd
--         else pure $ statusView (n + 1)
--
--
-- statusView :: Int -> View Status ()
-- statusView n = do
--   onLoad (CheckStatus n) 1000 $ do
--     el_ $ text $ "Checking Status" <> pack (show n)
--
--
-- lazyEnd :: View Status ()
-- lazyEnd = do
--   el_ "Lazy End"
--   target MainView $ do
--     button GoEnd (border 1) "Go End"
--
-- -- {-# LANGUAGE DeriveAnyClass #-}
-- -- {-# LANGUAGE LambdaCase #-}
-- -- {-# LANGUAGE OverloadedStrings #-}
-- -- {-# LANGUAGE TypeFamilies #-}
-- -- {-# LANGUAGE UndecidableInstances #-}
-- -- {-# OPTIONS_GHC -Wno-missing-signatures #-}
-- --
-- -- module Simple where
-- --
-- -- import Data.Text (Text, pack)
-- -- import Effectful
-- -- import Effectful.Concurrent.STM
-- -- import Effectful.Reader.Dynamic
-- -- import Effectful.State.Static.Local
-- -- import Web.Hyperbole
-- -- import Web.Hyperbole.Page
-- --
-- --
-- -- main = do
-- --   run 3000 $ do
-- --     liveApp (basicDocument "Example") (runReader @Int 5 $ page simplePage)
-- --
-- --
-- -- simplePage :: (Hyperbole :> es, IOE :> es, Reader Int :> es) => Page es '[MainView, Status]
-- -- simplePage = load $ do
-- --   liftIO $ putStrLn "MAIN LOAD"
-- --   pure $ col (pad 20) $ do
-- --     el bold "My Page"
-- --     hyper MainView $ do
-- --       row (gap 10) $ do
-- --         button GoBegin (border 1) "Start"
-- --
-- --
-- -- -- MAIN ----------------------------------------
-- --
-- -- data MainView = MainView
-- --   deriving (Show, Read, ViewId)
-- --
-- --
-- -- data MainAction
-- --   = GoBegin
-- --   | GoMid
-- --   | GoEnd
-- --   deriving (Show, Read, ViewAction)
-- --
-- --
-- -- instance HyperView MainView where
-- --   type Action MainView = MainAction
-- --   type Require MainView = '[Status]
-- --
-- --
-- -- instance (Reader Int :> es) => HandleView MainView es where
-- --   handle :: (Hyperbole :> es) => MainView -> MainAction -> Eff es (View MainView ())
-- --   handle _ action =
-- --     case action of
-- --       GoBegin -> do
-- --         n <- ask @Int
-- --         pure $ do
-- --           el_ $ text $ pack $ show n
-- --           beginStep
-- --       GoMid -> pure middleStep
-- --       GoEnd -> pure endStep
-- --
-- --
-- -- beginStep :: View MainView ()
-- -- beginStep = do
-- --   el_ "BEGIN"
-- --   button GoMid (border 1) " Mid"
-- --
-- --
-- -- middleStep :: View MainView ()
-- -- middleStep = do
-- --   el_ "MIDDLE: running"
-- --   button GoBegin (border 1) "Back"
-- --   hyper Status $ statusView 0
-- --
-- --
-- -- endStep :: View MainView ()
-- -- endStep = do
-- --   el_ "END"
-- --   button GoMid (border 1) "Back"
-- --
-- --
-- -- -- Status ---------------------------------------
-- --
-- -- data Status = Status deriving (Show, Read, ViewId)
-- -- data CheckStatus
-- --   = CheckStatus Int
-- --   deriving (Show, Read, ViewAction)
-- --
-- --
-- -- instance HyperView Status where
-- --   type Action Status = CheckStatus
-- --   type Require Status = '[MainView]
-- --
-- --
-- -- instance HandleView Status es where
-- --   handle _ = \case
-- --     CheckStatus n ->
-- --       if n >= 5
-- --         then pure lazyEnd
-- --         else pure $ statusView (n + 1)
-- --
-- --
-- -- statusView :: Int -> View Status ()
-- -- statusView n = do
-- --   onLoad (CheckStatus n) 1000 $ do
-- --     el_ $ text $ "Checking Status" <> pack (show n)
-- --
-- --
-- -- lazyEnd :: View Status ()
-- -- lazyEnd = do
-- --   el_ "Lazy End"
-- --   hyper MainView $ do
-- --     button GoEnd (border 1) "Go End"
-- --
-- --
