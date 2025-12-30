{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Example.Docs.CSS where

import Web.Atomic.CSS
import Web.Hyperbole

header = bold
h1 = header . fontSize 32
h2 = header . fontSize 24
h3 = header . fontSize 18
clickable = pointer . hover bold

example = do
  col $ do
    el ~ h3 $ "My Page"
    el ~ border 1 . pad 10 . clickable $ "Hover Me"
