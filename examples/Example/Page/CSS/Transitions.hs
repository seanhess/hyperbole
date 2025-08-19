module Example.Page.CSS.Transitions where

import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole

data Transitions = Transitions
  deriving (Generic, ViewId)

instance HyperView Transitions es where
  data Action Transitions
    = Expand
    | Collapse
    deriving (Generic, ViewAction)
  update Expand = do
    pure viewBig
  update Collapse = do
    pure viewSmall

viewSmall :: View Transitions ()
viewSmall = do
  col ~ gap 10 . transition 300 (Width 200) $ do
    el "Small"
    button Expand "Expand" ~ btn

viewBig :: View Transitions ()
viewBig =
  col ~ gap 10 . transition 300 (Width 400) $ do
    el "Expanded"
    button Collapse "Collapse" ~ btn
