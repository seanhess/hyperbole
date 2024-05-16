module Example.Forms where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity)
import Data.Text as T (Text, elem, length, pack)
import Effectful
import Example.Style qualified as Style
import GHC.Generics (Generic)
import Web.Hyperbole


page :: (Hyperbole :> es) => Page es Response
page = do
  hyper action

  load $ do
    pure $ row (pad 20) $ do
      viewId FormView (formView formInvalid)


data FormView = FormView
  deriving (Show, Read, Param)


data FormAction
  = Submit
  deriving (Show, Read, Param)


instance HyperView FormView where
  type Action FormView = FormAction


data UserForm a = UserForm
  { username :: Field a Text
  , age :: Field a Integer
  , password1 :: Field a Text
  , password2 :: Field a Text
  }
  deriving (Generic, Form)


action :: (Hyperbole :> es) => FormView -> FormAction -> Eff es (View FormView ())
action _ Submit = do
  u <- parseForm
  let inv = validateUser u
  -- I don't like this...
  case inv.username <|> inv.password1 <|> inv.age of
    Nothing -> pure $ userView u
    Just _ -> pure $ formView inv


valid :: Bool -> Text -> Maybe Text
valid True t = Just t
valid False _ = Nothing


-- another way to identify the fields? With labels? Doesn't have to be a record
-- maybe a sumtype? Then you could put them back in... ... perhaps...
validateUser :: UserForm Identity -> UserForm Invalid
validateUser u =
  UserForm
    { username = userNoSpaces <|> userLength
    , age = ageOld
    , password1 = passEqual <|> passLength
    , password2 = Nothing
    }
 where
  ageOld =
    valid (u.age < 2) "User must be at least 20 years old"

  userNoSpaces =
    valid (T.elem ' ' u.username) "Username must not contain spaces"

  userLength =
    valid (T.length u.username < 4) "Username must be at least 4 chars"

  passEqual =
    valid (u.password1 /= u.password2) "Passwords did not match"

  passLength =
    valid (T.length u.password1 < 8) "Password must be at least 8 chars"


formView :: UserForm Invalid -> View FormView ()
formView inv = do
  form @UserForm Submit (gap 10 . pad 10) $ \f -> do
    el Style.h1 "Sign Up"

    field (invalid inv.username) f.username $ do
      label "Username"
      input Username (inp . placeholder "username")
      maybe none (el_ . text) inv.username

    field (invalid inv.age) f.age $ do
      label "Age"
      input Number (inp . placeholder "age" . value "0")
      maybe none (el_ . text) inv.age

    field (invalid inv.password1) f.password1 $ do
      label "Password"
      input NewPassword (inp . placeholder "password")
      maybe none (el_ . text) inv.password1

    field id f.password2 $ do
      label "Repeat Password"
      input NewPassword (inp . placeholder "repeat password")

    submit Style.btn "Submit"
 where
  placeholder = att "placeholder"
  inp = border 1 . pad 8
  invalid Nothing = id
  invalid (Just _) = Style.invalid


userView :: UserForm Identity -> View FormView ()
userView user = do
  el (bold . Style.success) "Accepted Signup"
  row (gap 5) $ do
    el_ "Username:"
    el_ $ text user.username

  row (gap 5) $ do
    el_ "Age:"
    el_ $ text $ pack (show user.age)

  row (gap 5) $ do
    el_ "Password:"
    el_ $ text user.password1
