module Example.View.Inputs where

import Example.Colors
import Example.View.Icon qualified as Icon (check)
import Web.Hyperbole

toggleCheckBtn :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> View id ()
toggleCheckBtn clickAction isSelected = do
  button (clickAction (not isSelected)) circle contents
 where
  contents = if isSelected then Icon.check else " "
  circle = width 32 . height 32 . border 1 . rounded 100

progressBar :: Float -> View context () -> View context ()
progressBar pct content = do
  row (bg Light) $ do
    row (bg PrimaryLight . width (Pct pct) . pad 5) content
