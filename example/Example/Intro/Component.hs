module Example.Intro.Component where

import Data.Text (Text)
import Example.Colors
import Web.Hyperbole

styledButton :: (ViewAction (Action id)) => Action id -> Text -> View id ()
styledButton onClick lbl = do
  button onClick (pad 10 . bg Primary . hover (bg PrimaryLight) . rounded 5) (text lbl)
