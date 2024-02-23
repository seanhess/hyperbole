module Example.Transitions where

import Effectful
import Example.Colors
import Web.Hyperbole


-- this is already running in a different context
page :: (Hyperbole :> es) => Page es Response
page = do
  hyper content

  load $ do
    pure $ row (pad 20) $ do
      viewId Contents viewSmall


data Contents = Contents
  deriving (Show, Read, Param)
instance HyperView Contents where
  type Action Contents = ContentsAction


data ContentsAction
  = Expand
  | Collapse
  deriving (Show, Read, Param)


content :: (Hyperbole :> es) => Contents -> ContentsAction -> Eff es (View Contents ())
content _ Expand = do
  pure viewBig
content _ Collapse = do
  pure viewSmall


viewSmall :: View Contents ()
viewSmall = do
  col (gap 10 . border 1 . pad 20 . transition 300 (Height 200)) $ do
    el id "Hello"
    button Expand btn "Expand"


viewBig :: View Contents ()
viewBig = col (gap 10 . border 1 . pad 20 . transition 300 (Height 400)) $ do
  el_ "One"
  el_ "TWO"
  button Collapse (bg Secondary . hover (bg SecondaryLight) . color White . pad 10) "Collapse"


btn :: Mod
btn = bg Primary . hover (bg PrimaryLight) . color White . pad 10
