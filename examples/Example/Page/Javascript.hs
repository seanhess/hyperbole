module Example.Page.Javascript where

import Control.Monad (forM_)
import Data.Text (pack)
import Example.AppRoute qualified as Route
import Example.Colors
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole

-- TODO: show how to do tooltips with only CSS
-- TODO: a mouseover example that calls the server via runAction

page :: (Hyperbole :> es) => Eff es (Page '[Boxes])
page = do
  pure $ exampleLayout Route.Javascript $ do
    example "JS Mouse Overs" "Example/Page/Javascript.hs" $ do
      el "You can implement your own mouseovers in javascript and call the server via the JS API. You should debounce to avoid overloading the server!"
      -- NOTE: include custom javascript only on this page
      script "custom.js"
      col ~ embed $ do
        hyper Boxes $ boxesView Nothing

data Boxes = Boxes
  deriving (Generic, ViewId)

instance HyperView Boxes es where
  data Action Boxes
    = Selected Int
    | Clear
    deriving (Generic, ViewAction)

  update (Selected n) = do
    pure $ boxesView (Just n)
  update Clear = do
    pure $ boxesView Nothing

boxesView :: Maybe Int -> View Boxes ()
boxesView mn = do
  let ns = [0 .. 50] :: [Int]
  el ~ grid . gap 10 . pad 10 $ do
    el ~ double . border 2 . bold . fontSize 24 . pad 15 $ text $ pack $ maybe "" show mn
    forM_ ns $ \n -> do
      el @ onMouseEnter (Selected n) . onMouseLeave Clear ~ border 1 . pad 10 . pointer . hover (bg PrimaryLight) . textAlign AlignCenter $ text $ pack $ show n
 where
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
