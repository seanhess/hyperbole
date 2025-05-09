module Example.Page.Requests where

import Data.String.Conversions (cs)
import Effectful
import Example.AppRoute qualified as Route
import Example.View.Layout (exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Eff es (Page '[])
page = do
  r <- request
  pure $ exampleLayout Route.Requests $ do
    col ~ (gap 10 . pad 10) $ do
      el $ do
        text "Host: "
        text $ cs $ show r.host
      el $ do
        text "Path: "
        text $ cs $ show r.path
      el $ do
        text "Query: "
        text $ cs $ show r.query
      el $ do
        text "Cookies: "
        text $ cs $ show r.cookies
