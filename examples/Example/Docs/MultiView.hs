module Example.Docs.MultiView where

import Example.Counter (Counter (..), viewCount)
import Example.Simple (Message (..), messageView)
import Web.Hyperbole

page :: Page es [Message, Counter]
page = do
  pure $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World"
    hyper Counter $ viewCount 0
