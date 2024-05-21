module Example.Errors where

import Effectful
import Example.Style as Style
import Web.Hyperbole


-- this is already running in a different context
page :: (Hyperbole :> es) => Page es Response
page = do
  handle content

  load $ do
    pure $ row (pad 20) $ do
      col (gap 10 . border 1) $ do
        hyper Contents viewContent


data Contents = Contents
  deriving (Generic, Param)


data ContentsAction
  = CauseError
  deriving (Generic, Param)


instance HyperView Contents where
  type Action Contents = ContentsAction


content :: (Hyperbole :> es) => Contents -> ContentsAction -> Eff es (View Contents ())
content _ CauseError = do
  -- Return a not found error 404
  notFound


viewContent :: View Contents ()
viewContent = do
  col (gap 10 . pad 20) $ do
    button CauseError Style.btn "Not Found Error"
