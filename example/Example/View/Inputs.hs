module Example.View.Inputs where

import Example.View.Icon qualified as Icon (check)
import Web.Hyperbole

toggleCheckBtn :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> View id ()
toggleCheckBtn clickAction isSelected = do
  button (clickAction (not isSelected)) circle contents
 where
  contents = if isSelected then Icon.check else " "
  circle = width 32 . height 32 . border 1 . rounded 100
