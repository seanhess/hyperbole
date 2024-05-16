{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Forms where

import Data.Text as T (Text, elem, length, pack)
import Effectful
import Example.Style qualified as Style
import Web.Hyperbole


page :: (Hyperbole :> es) => Page es Response
page = do
  hyper action

  load $ do
    pure $ row (pad 20) $ do
      viewId FormView (formView mempty)


data FormView = FormView
  deriving (Show, Read, Param)


data FormAction = Submit
  deriving (Show, Read, Param)
instance HyperView FormView where
  type Action FormView = FormAction


-- Form Fields
data User = User Text deriving (Generic, FormField)
data Age = Age Int deriving (Generic, FormField)
data Pass1 = Pass1 Text deriving (Generic, FormField)
data Pass2 = Pass2 Text deriving (Generic, FormField)


action :: (Hyperbole :> es) => FormView -> FormAction -> Eff es (View FormView ())
action _ Submit = do
  u <- formField @User
  a <- formField @Age
  p1 <- formField @Pass1
  p2 <- formField @Pass2

  case validateUser u a p1 p2 of
    Validation [] -> pure $ userView u a p1
    errs -> pure $ formView errs


validateUser :: User -> Age -> Pass1 -> Pass2 -> Validation
validateUser (User u) (Age a) (Pass1 p1) (Pass2 p2) =
  validation
    [ validate @Age (a < 20) "User must be at least 20 years old"
    , validate @User (T.elem ' ' u) "Username must not contain spaces"
    , validate @User (T.length u < 4) "Username must be at least 4 chars"
    , validate @Pass1 (p1 /= p2) "Passwords did not match"
    , validate @Pass1 (T.length p1 < 8) "Password must be at least 8 chars"
    ]


formView :: Validation -> View FormView ()
formView v = do
  form Submit v (gap 10 . pad 10) $ do
    el Style.h1 "Sign Up"

    field @User id Style.invalid $ do
      label "Username"
      input Username (inp . placeholder "username")
      el_ invalidText

    field @Age id Style.invalid $ do
      label "Age"
      input Number (inp . placeholder "age" . value "0")
      el_ invalidText

    field @Pass1 id Style.invalid $ do
      label "Password"
      input NewPassword (inp . placeholder "password")
      el_ invalidText

    field @Pass2 id id $ do
      label "Repeat Password"
      input NewPassword (inp . placeholder "repeat password")

    submit Style.btn "Submit"
 where
  placeholder = att "placeholder"
  inp = border 1 . pad 8


-- inv :: forall a. (Field a) => Validation -> Mod
-- inv = onInvalid @a Style.invalid

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
