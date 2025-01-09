module Example.Docs.Component where

import Data.Text (Text)
import Example.Colors
import Web.Hyperbole

styledButton :: (ViewAction (Action id)) => Action id -> Text -> View id ()
styledButton clickAction lbl = do
  button clickAction btn (text lbl)
 where
  btn = pad 10 . bg Primary . hover (bg PrimaryLight) . rounded 5
