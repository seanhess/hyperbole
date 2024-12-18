module Example.Requests where

import Effectful
import Example.AppRoute qualified as Route
import Data.String.Conversions (cs)
import Example.View.Layout (exampleLayout)
import Web.Hyperbole
import Web.Hyperbole.Effect.Server (Request(..))


page :: (Hyperbole :> es) => Page es '[]
page = do
  r <- request
  pure $ exampleLayout Route.Requests $ do
    col (gap 10 . pad 10) $ do
      el_ $ do
        text "Host: "
        text $ cs $ show r.host
      el_ $ do
        text "Path: "
        text $ cs $ show r.path
      el_ $ do
        text "Query: "
        text $ cs $ show r.query
      el_ $ do
        text "Cookies: "
        text $ cs $ show $ fmap fst r.cookies

