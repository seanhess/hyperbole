module Example.Page.State.Stateless where

import Data.Text (pack)
import Docs.Page
import Example.AppRoute qualified as Route
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
