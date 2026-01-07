module Example.State.Stateless where

import App.Route qualified as Route
import Data.Text (pack)
import Docs.Page
import Example.Style.Cyber as Style
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView

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
