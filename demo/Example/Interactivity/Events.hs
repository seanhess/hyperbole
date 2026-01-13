module Example.Interactivity.Events where

import Data.Text (Text, pack)
import Example.Colors
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole hiding (button, input)

-- Try Events --------------------------------------

data TryEvents = TryEvents
  deriving (Generic, ViewId)

instance HyperView TryEvents es where
  data Action TryEvents
    = SetMessage Text
    deriving (Generic, ViewAction)

  update (SetMessage t) = do
    pure $ viewEvents t

viewEvents :: Text -> View TryEvents ()
viewEvents t = do
  el ~ bold $ text t
  input @ onInput SetMessage 250 ~ border 1 . pad 5 $ none
  button @ onDblClick (SetMessage "") ~ btn $ "Double Click to Clear"
 where
  input = tag "input"
  button = tag "button"

-- Boxes -----------------------------------

data Boxes = Boxes
  deriving (Generic, ViewId)

instance HyperView Boxes es where
  data Action Boxes
    = SelectBox Int
    | ClearBox
    deriving (Generic, ViewAction)

  -- favor the last action that happens
  type Concurrency Boxes = Replace

  update (SelectBox n) = do
    pure $ viewBoxes (Just n)
  update ClearBox = do
    pure $ viewBoxes Nothing

viewBoxes :: Maybe Int -> View Boxes ()
viewBoxes mn = do
  boxes mn $ \n -> do
    el ~ box @ onMouseEnter (SelectBox n) . onMouseLeave ClearBox $ text $ pack $ show n

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
