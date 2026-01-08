module Example.State.Stateless where

import Web.Hyperbole

data Swapper = Swapper
  deriving (Generic, ViewId)

instance HyperView Swapper es where
  data Action Swapper = Hello | Goodbye
    deriving (Generic, ViewAction)

  update Hello = pure "Hello"
  update Goodbye = pure "Goodbye"

page :: (Hyperbole :> es) => Page es '[Swapper]
page = do
  pure $ do
    hyper Swapper $ do
      button Hello "Hello"
      button Goodbye "Goodbye"
