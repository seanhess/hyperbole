module Example.Page.FormValidation where

import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Example.AppRoute qualified as Route
import Example.Style qualified as Style
import Example.View.Layout (exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole

page :: (Hyperbole :> es) => Eff es (Page '[FormView])
page = do
  pure $ exampleLayout Route.FormValidation $ row ~ pad 20 $ do
    hyper FormView (formView genFields)

data FormView = FormView
  deriving (Generic, ViewId)

instance HyperView FormView es where
  data Action FormView
    = Submit
    deriving (Generic, ViewAction)

  update Submit = do
    uf <- formData @(UserForm Identity)

    let vals = validateForm uf

    if anyInvalid vals
      then pure $ formView vals
      else pure $ userView uf

-- Form Fields
newtype User = User {username :: Text}
  deriving newtype (FromParam)

data UserForm f = UserForm
  { user :: Field f User
  , age :: Field f Int
  , pass1 :: Field f Text
  , pass2 :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields Validated, GenFields FieldName)

anyInvalid :: UserForm Validated -> Bool
anyInvalid u =
  or [isInvalid u.user, isInvalid u.age, isInvalid u.pass1, isInvalid u.pass2]

validateForm :: UserForm Identity -> UserForm Validated
validateForm u =
  UserForm
    { user = validateUser u.user
    , age = validateAge u.age
    , pass1 = validatePass u.pass1 u.pass2
    , pass2 = NotInvalid
    }

validateAge :: Int -> Validated Int
validateAge a =
  validate (a < 20) "User must be at least 20 years old"

validateUser :: User -> Validated User
validateUser (User u) =
  mconcat
    [ validate (T.elem ' ' u) "Username must not contain spaces"
    , validate (T.length u < 4) "Username must be at least 4 chars"
    , if u == "admin" || u == "guest"
        then Invalid "Username is already in use"
        else Valid
    ]

validatePass :: Text -> Text -> Validated Text
validatePass p1 p2 =
  mconcat
    [ validate (p1 /= p2) "Passwords did not match"
    , validate (T.length p1 < 8) "Password must be at least 8 chars"
    ]

formView :: UserForm Validated -> View FormView ()
formView val = do
  let f = fieldNames @UserForm
  form Submit ~ gap 10 . pad 10 $ do
    el ~ Style.h1 $ "Sign Up"

    field f.user ~ valStyle val.user $ do
      label "Username"
      input Username @ placeholder "username" ~ Style.input

      case val.user of
        Invalid t -> el (text t)
        Valid -> el "Username is available"
        _ -> none

    field f.age ~ valStyle val.age $ do
      label "Age"
      input Number @ placeholder "age" ~ Style.input
      el $ invalidText val.age

    field f.pass1 ~ valStyle val.pass1 $ do
      label "Password"
      input NewPassword @ placeholder "password" ~ Style.input
      el $ invalidText val.pass1

    field f.pass2 $ do
      label "Repeat Password"
      input NewPassword @ placeholder "repeat password" ~ Style.input

    submit "Submit" ~ Style.btn
 where
  valStyle (Invalid _) = Style.invalid
  valStyle Valid = Style.success
  valStyle _ = id

userView :: UserForm Identity -> View FormView ()
userView u = do
  el ~ bold . Style.success $ "Accepted Signup"
  row ~ gap 5 $ do
    el "Username:"
    el $ text u.user.username

  row ~ gap 5 $ do
    el "Age:"
    el $ text $ pack (show u.age)

  row ~ gap 5 $ do
    el "Password:"
    el $ text u.pass1
