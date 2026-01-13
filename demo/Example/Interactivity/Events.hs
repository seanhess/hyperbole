module Example.Interactivity.Events where

import Data.Text (pack)
import Example.Colors
import Web.Atomic.CSS
import Web.Hyperbole

data Boxes = Boxes
  deriving (Generic, ViewId)

instance HyperView Boxes es where
  data Action Boxes
    = Selected Int
    | Clear
    deriving (Generic, ViewAction)

  -- favor the last action that happens
  type Concurrency Boxes = Replace

  update (Selected n) = do
    pure $ viewBoxes (Just n)
  update Clear = do
    pure $ viewBoxes Nothing

viewBoxes :: Maybe Int -> View Boxes ()
viewBoxes mn = do
  boxes mn $ \n -> do
    el ~ box @ onMouseEnter (Selected n) . onMouseLeave Clear $ text $ pack $ show n

boxes :: Maybe Int -> (Int -> View c ()) -> View c ()
boxes mn boxView = do
  let ns = [0 .. 50] :: [Int]
  el ~ grid . gap 10 . pad 10 $ do
    col ~ double . border 2 . bold . fontSize 48 $ do
      space
      el ~ textAlign AlignCenter $ text $ pack $ maybe "" show mn
      space
    mapM_ boxView ns

box :: (Styleable h) => CSS h -> CSS h
box =
  border 1
    . pad 10
    . pointer
    . hover (bg PrimaryLight)
    . textAlign AlignCenter

grid :: (Styleable h) => CSS h -> CSS h
grid =
  utility
    "grid"
    [ "display" :. "grid"
    , "grid-template-columns" :. "repeat(auto-fit, minmax(50px, 1fr))"
    ]

double :: (Styleable h) => CSS h -> CSS h
double =
  utility
    "double"
    [ "grid-column" :. "1 / span 2"
    , "grid-row" :. "1 / span 2"
    ]
