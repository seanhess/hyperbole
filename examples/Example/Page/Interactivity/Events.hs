module Example.Page.Interactivity.Events where

import Control.Monad (forM_)
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

  update (Selected n) = do
    pure $ viewBoxes (Just n)
  update Clear = do
    pure $ viewBoxes Nothing

viewBoxes :: Maybe Int -> View Boxes ()
viewBoxes mn = do
  viewBoxes' mn $ \n -> do
    el ~ box @ onMouseEnter (Selected n) . onMouseLeave Clear $ text $ pack $ show n

viewBoxes' :: Maybe Int -> (Int -> View c ()) -> View c ()
viewBoxes' mn boxView = do
  let ns = [0 .. 50] :: [Int]
  el ~ grid . gap 10 . pad 10 $ do
    el ~ double . border 2 . bold . fontSize 24 . pad 15 $ text $ pack $ maybe "" show mn
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
