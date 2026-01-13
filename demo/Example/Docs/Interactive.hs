module Example.Docs.Interactive where

import Example.Simple
import Web.Hyperbole

page :: Page es '[Message]
page = do
  pure $ do
    el "Unchanging Header"
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World"
