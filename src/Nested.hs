{-# LANGUAGE LambdaCase #-}

module Page.Triggers.Nested where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Concurrent.STM
import Web.Hyperbole
import Web.Hyperbole.Component (Component (..))
import Web.Hyperbole.Effect.Hyperbole (start)


page :: (Hyperbole :> es, Concurrent :> es) => Page es '[MainView, Listener]
page = do
  pure $ col (pad 20 . gap 10) $ do
    el bold "Triggers Nested"
    start MainView $ MainViewModel{broadcast = Nothing}


data MainView = MainView
  deriving (Show, Read, ViewId)


instance Component MainView es where
  data Msg MainView
    = Broadcast Text
    | ClearAll
    deriving (Show, Read)


  data Model MainView = MainViewModel
    { broadcast :: Maybe Text
    }


  type Import MainView = '[Listener]


  render model = do
    let msg = fromMaybe "ready" model.broadcast
    row (gap 10) $ do
      col (gap 5) $ do
        button ClearAll Prelude.id "Clear"
        button (Broadcast "hello") Prelude.id "Broadcast hello"
        button (Broadcast "goodbye") Prelude.id "Broadcast goodbye"

      forM_ [0 .. 4] $ \i -> do
        start (Listener i) ListenerModel{msg}


  update = \case
    Broadcast t -> do
      pure $ render MainViewModel{broadcast = Just t}
    ClearAll -> do
      pure $ render MainViewModel{broadcast = Just ""}


data Listener = Listener Int
  deriving (Show, Read, ViewId)


instance Component Listener es where
  data Model Listener = ListenerModel
    { msg :: Text
    }


  data Msg Listener
    = Display Text
    | Shout Text
    deriving (Show, Read)


  -- Add additional effects required by the component
  type Effects Listener es = ()


  render :: Model Listener -> View Listener ()
  render model = do
    el (border 1 . textAlign Center) (text model.msg)
    row (gap 10) $ do
      button (Display "Hi") Prelude.id "Say Hi"
      button (Display "Bye") Prelude.id "Say Bye"
      button (Shout model.msg) Prelude.id "Shout"


  update = \case
    Display msg -> do
      pure $ render $ ListenerModel{msg}
    Shout msg -> do
      pure $ render $ ListenerModel{msg = Text.toUpper msg}
