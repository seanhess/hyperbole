module Example.Docs.Page.Messages where

import Web.Hyperbole

page :: Eff es (Page '[])
page = pure $ el_ "Messages page"
