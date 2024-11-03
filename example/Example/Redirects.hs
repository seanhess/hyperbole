module Example.Redirects where

import Effectful
import Example.Style as Style
import Web.Hyperbole


page :: (Hyperbole :> es) => Page es '[Contents]
page = do
  handle contents $ do
    pure $ row (pad 20) $ do
      hyper Contents contentsView


data Contents = Contents
  deriving (Show, Read, ViewId)


data ContentsAction
  = RedirectAsAction
  deriving (Show, Read, ViewAction)


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
