module Web.Hyperbole.Input where

import Data.Proxy
import Data.String (IsString)
import Data.Text
import Effectful
import GHC.TypeLits (KnownSymbol)
import Web.Hyperbole

data Form a = Form

newtype Label a = Label Text
  deriving newtype (IsString)

-- something need to carry the field name in it
input' :: Field x -> Mod -> View (Form a) ()
input' (Field n) f = tag "input" (f . name n) none

form' :: (HyperView action id) => action -> Mod -> View (Form a) () -> View id ()
form' a f cnt = do
  c <- context
  tag "form" (onSubmit a . dataTarget c . f . flexCol) $ addContext Form cnt

parseForm :: (Hyperbole :> es) => Field a -> Eff es a
parseForm _ = undefined

data Field a = Field Text
