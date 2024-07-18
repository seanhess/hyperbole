{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Forms where

import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import Example.Style qualified as Style
import Web.Hyperbole
import Web.Hyperbole.Forms


page :: (Hyperbole :> es) => Page es Response
page = do
  handle formAction

  load $ do
    pure $ row (pad 20) $ do
      hyper FormView (formView mempty)


data FormView = FormView
  deriving (Show, Read, ViewId)


data FormAction = Submit
  deriving (Show, Read, ViewAction)


instance HyperView FormView where
  type Action FormView = FormAction


-- Form Fields
data User = User Text deriving (Generic, FormField)
data Age = Age Int deriving (Generic, FormField)
data Pass1 = Pass1 Text deriving (Generic, FormField)
data Pass2 = Pass2 Text deriving (Generic, FormField)


type UserFields = [User, Age, Pass1, Pass2]


data UserForm = UserForm
  { user :: User
  , age :: Age
  , pass1 :: Pass1
  , pass2 :: Pass2
  }
  deriving (Generic, Form)


formAction :: (Hyperbole :> es) => FormView -> FormAction -> Eff es (View FormView ())
formAction _ Submit = do
  -- u <- formField @User
  -- a <- formField @Age
  -- p1 <- formField @Pass1
  -- p2 <- formField @Pass2
  uf <- formFields

  let vals = validateForm uf

  if anyInvalid vals
    then pure $ formView vals
    else pure $ userView uf.user uf.age uf.pass1


validateForm :: UserForm -> Validation UserFields
validateForm uf =
  validateUser uf.user <> validateAge uf.age <> validatePass uf.pass1 uf.pass2


validateAge :: Age -> Validation UserFields
validateAge (Age a) =
  validate @Age (a < 20) "User must be at least 20 years old"


validateUser :: User -> Validation UserFields
validateUser (User u) =
  mconcat
    [ validate @User (T.elem ' ' u) "Username must not contain spaces"
    , validate @User (T.length u < 4) "Username must be at least 4 chars"
    , validateWith @User $
        if u == "admin" || u == "guest"
          then Invalid "Username is already in use"
          else Valid
    ]


validatePass :: Pass1 -> Pass2 -> Validation UserFields
validatePass (Pass1 p1) (Pass2 p2) =
  mconcat
    [ validate @Pass1 (p1 /= p2) "Passwords did not match"
    , validate @Pass1 (T.length p1 < 8) "Password must be at least 8 chars"
    ]


formView :: Validation UserFields -> View FormView ()
formView v = do
  form Submit v (gap 10 . pad 10) $ do
    el Style.h1 "Sign Up"

    field @User valStyle $ do
      label "Username"
      input Username (inp . placeholder "username")

      fv <- fieldValid
      case fv of
        Invalid t -> el_ (text t)
        Valid -> el_ "Username is available"
        _ -> none

    field @Age valStyle $ do
      label "Age"
      input Number (inp . placeholder "age" . value "0")
      el_ invalidText

    field @Pass1 valStyle $ do
      label "Password"
      input NewPassword (inp . placeholder "password")
      el_ invalidText

    field @Pass2 (const id) $ do
      label "Repeat Password"
      input NewPassword (inp . placeholder "repeat password")

    submit Style.btn "Submit"
 where
  inp = Style.input
  valStyle (Invalid _) = Style.invalid
  valStyle Valid = Style.success
  valStyle _ = id


userView :: User -> Age -> Pass1 -> View FormView ()
userView (User user) (Age age) (Pass1 pass1) = do
  el (bold . Style.success) "Accepted Signup"
  row (gap 5) $ do
    el_ "Username:"
    el_ $ text user

  row (gap 5) $ do
    el_ "Age:"
    el_ $ text $ pack (show age)

  row (gap 5) $ do
    el_ "Password:"
    el_ $ text pass1
