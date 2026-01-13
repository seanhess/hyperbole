module Example.State.Stateless where

import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole

data Swapper = Swapper
  deriving (Generic, ViewId)

instance HyperView Swapper es where
  data Action Swapper = Hello | Goodbye
    deriving (Generic, ViewAction)

  update Hello = pure "Hello"
  update Goodbye = pure "Goodbye"

viewSwap :: View Swapper ()
viewSwap = do
  button Hello ~ btn $ "Hello"
  button Goodbye ~ btn $ "Goodbye"

page :: (Hyperbole :> es) => Page es '[Swapper]
page = do
  pure $ do
    hyper Swapper $ do
      button Hello "Hello"
      button Goodbye "Goodbye"
