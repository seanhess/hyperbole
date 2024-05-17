module Example.Redirects where

import Effectful
import Example.Style as Style
import Web.Hyperbole


page :: (Hyperbole :> es) => Page es Response
page = do
  hyper contents

  load $ do
    pure $ row (pad 20) $ do
      viewId Contents contentsView


data Contents = Contents
  deriving (Generic, Param)


data ContentsAction
  = RedirectAsAction
  deriving (Generic, Param)


instance HyperView Contents where
  type Action Contents = ContentsAction


contents :: (Hyperbole :> es) => Contents -> ContentsAction -> Eff es (View Contents ())
contents _ RedirectAsAction = do
  redirect "/hello/redirected"


contentsView :: View Contents ()
contentsView = do
  col (gap 10 . border 1 . pad 20 . transition 300 (Height 200)) $ do
    el id "Redirect as an Action"
    button RedirectAsAction Style.btn "Redirect Me"
