module Example.Errors where

import Effectful
import Example.Style as Style
import Web.Hyperbole


-- this is already running in a different context
page :: (Hyperbole :> es) => Page es '[Contents]
page = do
  pure $ row (pad 20) $ do
    col (gap 10 . border 1) $ do
      hyper Contents viewContent


data Contents = Contents
  deriving (Show, Read, ViewId)


instance HyperView Contents es where
  data Action Contents
    = CauseError
    deriving (Show, Read, ViewAction)


  update CauseError = do
    -- Return a not found error 404
    notFound


viewContent :: View Contents ()
viewContent = do
  col (gap 10 . pad 20) $ do
    button CauseError Style.btn "Not Found Error"

-- Compile Errors (Uncomment)
