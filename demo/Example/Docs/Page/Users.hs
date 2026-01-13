module Example.Docs.Page.Users where

import Web.Hyperbole

page :: Int -> Page es '[]
page _ = pure $ el "User page"
