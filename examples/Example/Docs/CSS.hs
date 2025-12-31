{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Example.Docs.CSS where

import Web.Atomic.CSS
import Web.Hyperbole

example = do
  col $ do
    el ~ h3 $ "My Page"
    el ~ btn $ "Hover Me"
 where
  header = bold
  h1 = header . fontSize 32
  h2 = header . fontSize 24
  h3 = header . fontSize 18

  btn =
    pad 10 . border 1 . pointer . hover (bold . border 2)
