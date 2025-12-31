{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.SideEffects where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Web.Atomic.CSS
import Web.Hyperbole

-- page :: (Hyperbole :> es, IOE :> es) => Page es '[]
-- page = do
--   runIO $ putStrLn "HELLO"
--   let msg = fromMaybe "hello" prm
--   pure $ do
--     hyper Message $ messageView msg

-- page :: (Hyperbole :> es, IOE :> es) => Page es '[Message]
-- page = do
--   prm <- lookupParam "msg"
--   let msg = fromMaybe "hello" prm
--   pure $ do
--     hyper Message $ messageView msg

-- data Message = Message
--   deriving (Generic, ViewId)
--
-- instance (IOE :> es) => HyperView Message es where
--   data Action Message
--     = Louder Text
--     deriving (Generic, ViewAction)
--
--   update (Louder msg) = do
--     let new = msg <> "!"
--     setParam "msg" new
--     pure $ messageView new
--
-- messageView :: Text -> View Message ()
-- messageView m = do
--   button (Louder m) ~ border 1 $ "Louder"
--   el ~ bold $ text $ "Message: " <> m

-- data Message = Message
--   deriving (Generic, ViewId)
--
-- instance (IOE :> es) => HyperView Message es where
--   data Action Message
--     = Louder Text
--     deriving (Generic, ViewAction)
--
--   update (Louder msg) = do
--     let new = msg <> "!"
--     setParam "msg" new
--     pure $ messageView new
--
-- messageView :: Text -> View Message ()
-- messageView m = do
--   button (Louder m) ~ border 1 $ "Louder"
--   el ~ bold $ text $ "Message: " <> m

data Titler = Titler
  deriving (Generic, ViewId)

instance HyperView Titler es where
  data Action Titler
    = SetTitle Text
    deriving (Generic, ViewAction)

  update (SetTitle msg) = do
    pageTitle msg
    pure "Check the title"
