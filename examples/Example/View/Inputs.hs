module Example.View.Inputs where

import Example.Colors
import Web.Hyperbole

toggleCheckbox :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> View id ()
toggleCheckbox clickAction isSelected = do
  tag "input" (att "type" "checkbox" . checked isSelected . onClick (clickAction (not isSelected)) . big) none
 where
  big = width 32 . height 32

progressBar :: Float -> View context () -> View context ()
progressBar pct content = do
  row (bg Light) $ do
    row (bg PrimaryLight . width (Pct pct) . pad 5) content
