module Example.Forms where

import Data.Functor.Identity (Identity)
import Data.Text (Text, pack)
import Effectful
import Example.Colors
import GHC.Generics (Generic)
import Web.Hyperbole
import Web.Hyperbole.Input

data Main = Main
  deriving (Show, Read, Param)

data MainAction
  = Submit
  | Cancel
  deriving (Show, Read, Param)

instance HyperView Main where
  type Action Main = MainAction

-- need to be able to set bg color of page, sure
page :: (Hyperbole :> es) => Page es ()
page = do
  hyper action

  load $ do
    pure $ row (pad 20) $ do
      viewId Main formView

action :: (Hyperbole :> es) => Main -> MainAction -> Eff es (View Main ())
action _ Submit = do
  u <- parseForm @UserForm
  pure $ userView u
action _ Cancel = do
  pure $ el_ "Cancelled"

userView :: UserForm Identity -> View Main ()
userView user = do
  el_ "User View"
  el_ $ text user.username
  el_ $ text $ pack (show user.age)
  el_ $ text user.password1
  el_ $ text user.password2

btn :: Mod
btn = bg Primary . hover (bg PrimaryLight) . color White . pad 10

h1 :: Mod
h1 = bold . fontSize 32

formView :: View Main ()
formView = do
  form' @UserForm Submit (gap 10 . pad 10) $ \f -> do
    el h1 "Sign Up"

    -- TODO: limit certain form inputs to certain types
    -- TODO: labels
    input' Username f.username (inp . placeholder "username")
    input' Number f.age (inp . placeholder "age")
    input' NewPassword f.password2 (inp . placeholder "repeat password")
    input' NewPassword "woot" (inp . placeholder "repeat password")

    submit id "Submit"
    button Cancel id "Cancel"
 where
  placeholder = att "placeholder"
  inp = border 1 . pad 8

data UserForm a = UserForm
  { username :: Field a Text
  , age :: Field a Integer
  , password1 :: Field a Text
  , password2 :: Field a Text
  }
  deriving (Generic, Form)
