{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Forms where

import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Example.Style qualified as Style
import Web.Hyperbole


page :: (Hyperbole :> es) => Page es Response
page = do
  handle formAction

  load $ do
    pure $ row (pad 20) $ do
      hyper FormView (formView genForm)


data FormView = FormView
  deriving (Show, Read, ViewId)


data FormAction = Submit
  deriving (Show, Read, ViewAction)


instance HyperView FormView where
  type Action FormView = FormAction


-- Form Fields
newtype User = User {username :: Text}
  deriving (Generic)
  deriving newtype (FromHttpApiData)


data UserForm f = UserForm
  { user :: Field f User
  , age :: Field f Int
  , pass1 :: Field f Text
  , pass2 :: Field f Text
  }
  deriving (Generic)
instance Form UserForm Validated


formAction :: (Hyperbole :> es) => FormView -> FormAction -> Eff es (View FormView ())
formAction _ Submit = do
  uf <- formData @UserForm

  let vals = validateForm uf

  if anyInvalid vals
    then pure $ formView vals
    else pure $ userView uf


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
formView v = do
  let f = formFieldsWith v
  form @UserForm Submit (gap 10 . pad 10) $ do
    el Style.h1 "Sign Up"

    -- what does it need from this? the field name, is the only thing, really
    -- what about a simpler thing...
    -- people could always just set the id... and parse the id manually
    field f.user valStyle $ do
      label "Username"
      input Username (inp . placeholder "username")

      fv <- fieldValid
      case fv of
        Invalid t -> el_ (text t)
        Valid -> el_ "Username is available"
        _ -> none

    field f.age valStyle $ do
      label "Age"
      input Number (inp . placeholder "age" . value "0")
      el_ invalidText

    field f.pass1 valStyle $ do
      label "Password"
      input NewPassword (inp . placeholder "password")
      el_ invalidText

    field f.pass2 (const id) $ do
      label "Repeat Password"
      input NewPassword (inp . placeholder "repeat password")

    submit Style.btn "Submit"
 where
  inp = Style.input
  valStyle (Invalid _) = Style.invalid
  valStyle Valid = Style.success
  valStyle _ = id


formViewEmpty :: View FormView ()
formViewEmpty = do
  let f = formFields @UserForm
  form @UserForm Submit (gap 10 . pad 10) $ do
    el Style.h1 "Sign Up"

    field f.user valStyle $ do
      label "Username"
      input Username (inp . placeholder "username")

      fv <- fieldValid
      case fv of
        Invalid t -> el_ (text t)
        Valid -> el_ "Username is available"
        _ -> none

    field f.age valStyle $ do
      label "Age"
      input Number (inp . placeholder "age" . value "0")
      el_ invalidText

    field f.pass1 valStyle $ do
      label "Password"
      input NewPassword (inp . placeholder "password")
      el_ invalidText

    field f.pass2 (const id) $ do
      label "Repeat Password"
      input NewPassword (inp . placeholder "repeat password")

    submit Style.btn "Submit"
 where
  inp = Style.input
  valStyle (Invalid _) = Style.invalid
  valStyle Valid = Style.success
  valStyle _ = id


userView :: UserForm Identity -> View FormView ()
userView u = do
  el (bold . Style.success) "Accepted Signup"
  row (gap 5) $ do
    el_ "Username:"
    el_ $ text u.user.username

  row (gap 5) $ do
    el_ "Age:"
    el_ $ text $ pack (show u.age)

  row (gap 5) $ do
    el_ "Password:"
    el_ $ text u.pass1
