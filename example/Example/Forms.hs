module Example.Forms where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Effectful
import Example.Colors
import GHC.TypeLits (KnownSymbol, Symbol)
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
  u <- parseUser
  pure $ userView u

userView :: User -> View Main ()
userView user = do
  el_ "User View"
  el_ $ text user.name

btn :: Mod
btn = bg Primary . hover (bg PrimaryLight) . color White . pad 10

h1 :: Mod
h1 = bold . fontSize 32

nameField :: Field Text
nameField = Field "name"

ageField :: Field Int
ageField = Field "age"

parseUser :: (Hyperbole :> es) => Eff es User
parseUser = do
  n <- parseForm nameField
  a <- parseForm ageField
  pure $ User n a

formView :: View Main ()
formView = do
  form' Submit (gap 10 . pad 10) $ do
    el h1 "Sign Up"

    input' nameField id
    input' ageField id
    submit id "Submit"

data User = User
  { name :: Text
  , age :: Int
  }
