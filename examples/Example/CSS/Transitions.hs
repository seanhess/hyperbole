{-# LANGUAGE TemplateHaskell #-}

module Example.CSS.Transitions where

import Docs.Examples
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole

source :: ExampleSource
source = $(exampleSource)

data Animate = Animate
  deriving (Generic, ViewId)

instance HyperView Animate es where
  data Action Animate
    = Expand
    | Collapse
    deriving (Generic, ViewAction)
  update Expand = do
    pure viewBig
  update Collapse = do
    pure viewSmall

viewSmall :: View Animate ()
viewSmall = do
  col ~ gap 10 . transition 300 (Width 200) $ do
    el "Small"
    button Expand "Expand" ~ btn

viewBig :: View Animate ()
viewBig =
  col ~ gap 10 . transition 300 (Width 400) $ do
    el "Expanded"
    button Collapse "Collapse" ~ btn
