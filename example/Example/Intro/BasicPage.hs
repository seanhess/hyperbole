{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Intro.BasicPage where

import Web.Hyperbole
import Web.Hyperbole.Effect.Handler
import Data.Text (Text)

main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage messagePage)

main' = do
  run 3000 $ do
    liveApp (basicDocument "Example") (loadToResponse messagePage')

messagePage :: Eff es (Page '[])
messagePage = do
  pure $ do
    col (pad 10) $ do
      el bold "Hello World"

messagePage' :: Eff es (Page '[Message])
messagePage' = pure $ messageView1

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
