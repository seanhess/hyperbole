module Example.Page.Errors where

import Effectful
import Example.AppRoute qualified as Route
import Example.Style as Style
import Example.View.Layout (exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Eff es (Page '[Contents])
page = do
  pure $ exampleLayout Route.Errors $ row ~ pad 20 $ do
    col ~ gap 10 . border 1 $ do
      hyper Contents viewContent

data Contents = Contents
  deriving (Generic, ViewId)

instance HyperView Contents es where
  data Action Contents
    = CauseError
    deriving (Generic, ViewAction)

  update CauseError = do
    -- Return a not found error 404
    notFound

viewContent :: View Contents ()
viewContent = do
  col ~ gap 10 . pad 20 $ do
    button CauseError ~ Style.btn $ "Not Found Error"

-- Compile Errors (Uncomment)
