module Example.Docs.Page.Users where

import Web.Hyperbole

page :: Int -> Eff es (Page '[])
page _ = pure $ el "User page"
