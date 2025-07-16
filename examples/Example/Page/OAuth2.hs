module Example.Page.OAuth2 (page) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Example.View.Layout
import Web.Hyperbole

import qualified Example.AppRoute as Route

--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------

page :: (Hyperbole :> es) => Eff es (Page '[])
page = do
  pure $ exampleLayout Route.OAuth2 $ do
    example "OAuth2" "Example/Page/OAuth2.hs" $ do
      el "Hello, World!"
