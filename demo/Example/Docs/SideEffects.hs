{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.SideEffects where

import Data.Text (Text)
import App.Docs
import Effectful
import Effectful.Concurrent
import Effectful.Reader.Dynamic
import Example.Colors
import Example.Style.Cyber
import Web.Atomic.CSS
import Web.Hyperbole

-- page :: (Hyperbole :> es, Concurrent :> es) => Page es '[]
-- page = do
--   threadDelay 1000
--   let msg = fromMaybe "hello" prm
--   pure $ do
--     hyper Message $ messageView msg

-- page :: (Hyperbole :> es, IOE :> es) => Page es '[Message]
-- page = do
--   prm <- lookupParam "msg"
--   let msg = fromMaybe "hello" prm
--   pure $ do
--     hyper Message $ messageView msg

app :: Application
app = do
  liveApp quickStartDocument $ do
    runConcurrent . runReader @Text "Secret!" $
      runPage page

page :: (Hyperbole :> es, Concurrent :> es, Reader Text :> es) => Page es '[SlowReader]
page = do
  pure $ hyper SlowReader $ messageView "..."

data SlowReader = SlowReader
  deriving (Generic, ViewId)

instance (Concurrent :> es, Reader Text :> es) => HyperView SlowReader es where
  data Action SlowReader
    = GetMessage
    deriving (Generic, ViewAction)

  update GetMessage = do
    threadDelay 500000
    msg <- ask
    pure $ messageView msg

messageView :: Text -> View SlowReader ()
messageView m = do
  el ~ bold . whenLoading (color SecondaryLight) $ text $ "Message: " <> m
  button GetMessage ~ btn $ "Get Message from Reader"

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

titleView :: View Titler ()
titleView = do
  button (SetTitle "Hello") ~ btn $ "Set Title"

source :: ModuleSource
source = $(moduleSource)
