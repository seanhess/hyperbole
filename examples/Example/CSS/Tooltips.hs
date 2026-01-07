{-# LANGUAGE TemplateHaskell #-}

module Example.CSS.Tooltips where

import Docs.Examples
import Example.Colors
import Web.Atomic.CSS
import Web.Hyperbole

source :: ModuleSource
source = $(moduleSource)

tooltips :: View c ()
tooltips = do
  col ~ pad 10 . gap 10 . width 300 $ do
    mapM_ viewItemRow ["One", "Two", "Three", "Four", "Five", "Six"]
 where
  viewItemRow item = do
    col ~ stack . showTooltips . hover (color Primary) . pointer $ do
      el ~ border 1 . bg White . pad 5 $ text item
      el ~ cls "tooltip" . popup (TR 10 10) . zIndex 1 . visibility Hidden $ do
        col ~ border 2 . gap 5 . bg White . pad 5 $ do
          el ~ bold $ "DETAILS"
          el $ text item
          el "details about this item"

  showTooltips =
    css
      "tooltips"
      ".tooltips:hover > .tooltip"
      (declarations (visibility Visible))
