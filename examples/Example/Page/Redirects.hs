{-# LANGUAGE QuasiQuotes #-}

module Example.Page.Redirects where

import Effectful
import Example.AppRoute qualified as Route
import Example.Style as Style
import Example.View.Layout (exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Data.URI

page :: (Hyperbole :> es) => Eff es (Page '[Contents])
page = do
  pure $ exampleLayout Route.Redirects $ row ~ pad 20 $ do
    hyper Contents contentsView

data Contents = Contents
  deriving (Generic, ViewId)

instance HyperView Contents es where
  data Action Contents = RedirectAsAction
    deriving (Generic, ViewAction)
  update RedirectAsAction = do
    redirect $ pathUri "/hello/redirected"

contentsView :: View Contents ()
contentsView = do
  col ~ gap 10 . border 1 . pad 20 . transition 300 (Height 200) $ do
    el "Redirect as an Action"
    button RedirectAsAction ~ Style.btn $ "Redirect Me"
