module Example.Forms where

import Data.Functor.Identity (Identity)
import Data.Text (Text, pack)
import Effectful
import Example.Colors
import GHC.Generics (Generic)
import Web.Hyperbole


page :: (Hyperbole :> es) => Page es ()
page = do
  hyper action

  load $ do
    pure $ row (pad 20) $ do
      viewId Main formView


data Main = Main
  deriving (Show, Read, Param)


data MainAction
  = Submit
  | Cancel
  deriving (Show, Read, Param)


instance HyperView Main where
  type Action Main = MainAction


data UserForm a = UserForm
  { username :: Field a Text
  , age :: Field a Integer
  , password1 :: Field a Text
  , password2 :: Field a Text
  }
  deriving (Generic, Form)


action :: (Hyperbole :> es) => Main -> MainAction -> Eff es (View Main ())
action _ Submit = do
  u <- parseForm
  pure $ userView u
action _ Cancel = do
  pure $ el_ "Cancelled"


formView :: View Main ()
formView = do
  form @UserForm Submit (gap 10 . pad 10) $ \f -> do
    el h1 "Sign Up"

    -- TODO: labels are required: `field`
    -- WARNING: there's no way to know you've implemented all the fields. Need better error reporting
    -- or we need to provide a record TO form, with each field filled in, but that messes with layout... so... no thanks
    input Username (inp . placeholder "username") f.username
    input Number (inp . placeholder "age") f.age
    input NewPassword (inp . placeholder "repeat password") f.password1
    input NewPassword (inp . placeholder "repeat password") f.password2

    submit id "Submit"
    button Cancel id "Cancel"
 where
  placeholder = att "placeholder"
  inp = border 1 . pad 8


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
