{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Intro.BasicPage where

import Web.Hyperbole
import Data.Text (Text)

main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage messagePage1)

main2 = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage messagePage2)

messagePage1 :: Eff es (Page '[])
messagePage1 = do
  pure $ do
    col (pad 10) $ do
      el bold "Hello World"

messagePage2 :: Eff es (Page '[])
messagePage2 = pure $ messageView1

data Message = Message
  deriving (Show, Read, ViewId)

-- We need the `anything` here to let the context changed to `Root` for a `Page`.
messageView1 :: View anything ()
messageView1 =
  col (pad 10) $ do
    el bold "Hello World"

messageView2 :: Text -> View Message ()
messageView2 t =
  col (pad 10) $ do
    el bold $ text $ "Hello World, dear " <> t
