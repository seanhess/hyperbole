module Example.View.Inputs where

import Example.Colors
import Web.Hyperbole

toggleCheckBtn :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> View id ()
toggleCheckBtn clickAction isSelected = do
  toggle isSelected clickAction circle
 where
  -- contents = if isSelected then Icon.check else " "
  circle = width 32 . height 32 . border 1 . rounded 100

progressBar :: Float -> View context () -> View context ()
progressBar pct content = do
  row (bg Light) $ do
    row (bg PrimaryLight . width (Pct pct) . pad 5) content
