{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.Component where

import Data.Kind (Constraint, Type)
import Effectful
import Effectful.Reader.Dynamic (Reader)
import Web.View


-- | A 'Component' is a self-contained piece of a 'Page'.
class Component c es where
  -- | The data used by the component
  data Model c


  -- | The possible messages supported by the component
  data Action c


  -- | Additional effects required by the component. Can be omitted if none are needed.
  type Effects c (es :: [Effect]) :: Constraint


  type Effects c es = ()


  -- | Other components nested in the component.
  -- Can be omitted if none are needed.
  type Require c :: [Type]


  type Require c = '[]


  -- | Render the component
  render :: Model c -> View c ()


  -- | Update the component based on a message. Ideally would only change the data model and leave rendering to the 'render' function.
  update :: (Effects c es) => Action c -> Eff (Reader c : es) (View c ())
