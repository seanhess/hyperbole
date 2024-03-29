module Example.Forms where

import Data.Functor.Identity (Identity)
import Data.Text (Text, pack)
import Effectful
import Example.Style as Style
import GHC.Generics (Generic)
import Web.Hyperbole


page :: (Hyperbole :> es) => Page es Response
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
    el Style.h1 "Sign Up"

    field id $ do
      label "Username"
      input Username (inp . placeholder "username") f.username

    field id $ do
      label "Age"
      input Number (inp . placeholder "age") f.age

    field id $ do
      label "Password"
      input NewPassword (inp . placeholder "password") f.password1

    field id $ do
      label "Repeat Password"
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
