{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.Component where

import Data.Kind (Constraint, Type)
import Effectful
import Effectful.Reader.Dynamic (Reader)
import Web.View


-- | A 'Component' is a self-contained piece of a 'Page'.
class Component c es where
  data Model c
  data Msg c


  -- | Additional effects required by the component. Can be omitted if none are needed.
  type Effects c (es :: [Effect]) :: Constraint


  type Effects c es = ()


  -- | Other components required by this component
  type Import c :: [Type]


  type Import c = '[]


  render :: Model c -> View c ()
  update :: (Effects c es) => Msg c -> Eff (Reader c : es) (View c ())
