module Example.State.ViewState where

import Data.Text (pack)
import Example.Style.Cyber (btn, dataFeature)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView

page :: (Hyperbole :> es) => Page es '[Counter]
page = do
  pure $ do
    hyperState Counter 1 viewCount

data Counter = Counter
  deriving (Generic)
instance ViewId Counter where
  type ViewState Counter = Int

instance HyperView Counter es where
  data Action Counter
    = Increment
    | Decrement
    deriving (Generic, ViewAction)

  update Increment = do
    modify @Int (+ 1)
    pure viewCount
  update Decrement = do
    modify @Int (subtract 1)
    pure viewCount

viewCount :: View Counter ()
viewCount = row $ do
  n <- viewState
  col ~ gap 10 $ do
    el ~ dataFeature $ text $ pack $ show n
    row ~ gap 10 $ do
      button Decrement "Decrement" ~ btn
      button Increment "Increment" ~ btn
