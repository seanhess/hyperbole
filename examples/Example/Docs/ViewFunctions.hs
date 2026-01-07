{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Docs.ViewFunctions where

import Data.Text (Text)
import Docs.Examples
import Example.Style.Cyber (btn)
import Example.View.Inputs (progressBar, toggleCheckbox)
import Web.Atomic.CSS
import Web.Hyperbole

page :: Page es '[Message]
page = do
  pure $ do
    hyper VFMessage $ messageView "Hello"

data Message = VFMessage
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message
    = SetMessage Text
    deriving (Generic, ViewAction)

  update (SetMessage t) =
    pure $ messageView t

messageView :: Text -> View Message ()
messageView m = do
  header m
  messageButton "Salutations!"
  messageButton "Good Morning!"
  messageButton "Goodbye"

messageButton :: Text -> View Message ()
messageButton msg = do
  button (SetMessage msg) ~ btn $ text $ "Say " <> msg

header :: Text -> View ctx ()
header txt = do
  el ~ bold $ text txt

source :: ModuleSource
source = $(moduleSource)

-- Toggle Examples ----------------------------

data Toggler = Toggler
  deriving (Generic, ViewId)

instance HyperView Toggler es where
  data Action Toggler
    = Toggle Bool
    deriving (Generic, ViewAction)

  update (Toggle b) =
    -- do something with the data
    pure $ toggler b

toggler :: Bool -> View Toggler ()
toggler b =
  row ~ gap 10 $ do
    toggleCheckbox Toggle b
    text "I am using view functions"

-- Progress Example ------------------------

data Progress = Progress
  deriving (Generic, ViewId)

instance HyperView Progress es where
  data Action Progress
    = MakeProgress Float
    deriving (Generic, ViewAction)

  update (MakeProgress pct) =
    pure $ workingHard (pct + 0.1)

workingHard :: Float -> View Progress ()
workingHard prog =
  row ~ gap 10 $ do
    button (MakeProgress prog) ~ btn $ " + Progress"
    progressBar prog ~ grow $ do
      el ~ grow . fontSize 18 $
        if prog >= 1
          then "Done!"
          else "Working..."
