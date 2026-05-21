module Example.Docs.Client where

import Web.Hyperbole

page :: (Hyperbole :> es) => Page es '[]
page = do
  pageTitle "My Page Title"
  pure $ el "Hello World"
