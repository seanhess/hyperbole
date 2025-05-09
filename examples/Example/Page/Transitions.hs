module Example.Page.Transitions where

import Effectful
import Example.AppRoute
import Example.Style as Style
import Example.View.Layout (exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Eff es (Page '[Contents])
page = do
  pure $ exampleLayout Transitions $ do
    col ~ pad 10 $ do
      col ~ border 1 . pad 20 $ do
        hyper Contents viewSmall

data Contents = Contents
  deriving (Generic, ViewId)

data ContentsAction

instance HyperView Contents es where
  data Action Contents
    = Expand
    | Collapse
    deriving (Generic, ViewAction)
  update Expand = do
    pure viewBig
  update Collapse = do
    pure viewSmall

viewSmall :: View Contents ()
viewSmall = do
  col ~ gap 10 . transition 300 (Width 200) $ do
    el "Small"
    button Expand "Expand" ~ Style.btn

viewBig :: View Contents ()
viewBig =
  col ~ gap 10 . transition 300 (Width 400) $ do
    el "Expanded"
    button Collapse "Collapse" ~ Style.btn
