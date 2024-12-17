module Example.Transitions where

import Effectful
import Example.Style as Style
import Web.Hyperbole


page :: (Hyperbole :> es) => Page es '[Contents]
page = do
  pure $ row (pad 20) $ do
    hyper Contents viewSmall


data Contents = Contents
  deriving (Show, Read, ViewId)


data ContentsAction


instance HyperView Contents es where
  data Action Contents
    = Expand
    | Collapse
    deriving (Show, Read, ViewAction)
  update Expand = do
    pure viewBig
  update Collapse = do
    pure viewSmall


viewSmall :: View Contents ()
viewSmall = do
  col (gap 10 . border 1 . pad 20 . transition 300 (Width 200)) $ do
    el id "Small"
    button Expand Style.btn "Expand"


viewBig :: View Contents ()
viewBig = col (gap 10 . border 1 . pad 20 . transition 300 (Width 400)) $ do
  el_ "Expanded"
  button Collapse Style.btn "Collapse"
