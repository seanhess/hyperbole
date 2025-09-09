module Example.View.Inputs where

import Example.Colors
import Web.Atomic.CSS
import Web.Hyperbole

toggleCheckbox :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> View id ()
toggleCheckbox setChecked isSelected = do
  tag "input" @ att "type" "checkbox" . onClick (setChecked (not isSelected)) . checked isSelected ~ big $ none
 where
  big = width 32 . height 32

progressBar :: Float -> View context () -> View context ()
progressBar pct contents = do
  row ~ bg Light $ do
    row ~ bg PrimaryLight . width (Pct pct) . pad 5 $ contents
