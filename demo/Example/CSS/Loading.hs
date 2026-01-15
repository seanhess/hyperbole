{-# LANGUAGE UndecidableInstances #-}

module Example.CSS.Loading where

import Data.Text (Text)
import Example.Effects.Debug
import Example.Style.Cyber (btn)
import Web.Atomic.CSS
import Web.Hyperbole

data Loader = Loader
  deriving (Generic, ViewId)

instance (Debug :> es) => HyperView Loader es where
  data Action Loader
    = LoadSlow
    deriving (Generic, ViewAction)

  update LoadSlow = do
    delay 1000
    pure $ viewLoaders "OK!"

viewLoaders :: Text -> View Loader ()
viewLoaders status = do
  col ~ gap 10 $ do
    row ~ gap 10 . whenLoading flexRow . display None $ do
      loadingBars
      el "Loading..."
    el ~ whenLoading (display None) $ text status
    button LoadSlow ~ btn . whenLoading (opacity 0.5) $ "Load Slow"

loadingBars :: View c ()
loadingBars = el ~ cls "loader" $ none
