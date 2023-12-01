module Example.Forms where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Effectful
import Example.Colors
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm (..))
import Web.Hyperbole
import Web.Hyperbole.Input

data Main = Main
  deriving (Show, Read, Param, HyperView Action)

data Action
  = Submit
  deriving (Show, Read, Param)

-- need to be able to set bg color of page, sure
page :: (Hyperbole :> es) => Page es ()
page = do
  hyper action

  load $ do
    pure $ row (pad 20) $ do
      viewId Main formView

action :: (Hyperbole :> es) => Main -> Action -> Eff es (View Main ())
action _ Submit = do
  u <- parseForm @UserForm
  pure $ userView u

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
    el h1 "Sign Up?"

    -- TODO: limit certain form inputs to certain types
    input' f.username (inp . placeholder "username")
    input' f.age (inp . placeholder "age")
    input' f.password1 (inp . placeholder "repeat password")
    input' f.password2 (inp . placeholder "repeat password")

    submit id "Submit"
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
