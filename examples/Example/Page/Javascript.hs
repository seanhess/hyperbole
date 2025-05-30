module Example.Page.Javascript where

import Data.Text (Text)
import Example.AppRoute qualified as Route
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

-- TODO: show how to do tooltips with only CSS
-- TODO: a mouseover example that calls the server via runAction

main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage page)

page :: (Hyperbole :> es) => Eff es (Page '[Message, Nope])
page = do
  pure $ exampleLayout Route.Javascript $ do
    example "Javascript" "Example/Page/Javascript.hs" $ do
      -- NOTE: include custom javascript only on this page
      script "custom.js"
      col ~ embed $ do
        hyper Message1 $ messageView "Hello"
        hyper Message2 $ messageView "World!"

data Message = Message1 | Message2 | Message3
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message
    = Louder Text
    | Reset Text
    deriving (Generic, ViewAction)

  update (Reset t) = do
    pure $ messageView t
  update (Louder m) = do
    let new = m <> "!"
    pure $ messageView new

messageView :: Text -> View Message ()
messageView m = do
  row ~ gap 10 $ do
    button (Louder m) "Louder" ~ border 1 . pad 5
    el ~ pad 5 $ text m

data Nope = Nope
  deriving (Generic, ViewId)

instance HyperView Nope es where
  data Action Nope = Noop
    deriving (Generic, ViewAction)
  update _ = pure none
