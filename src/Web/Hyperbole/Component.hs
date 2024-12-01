{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Component where

import Data.Kind (Constraint, Type)
import Data.Text (Text, pack, unpack)
import Effectful
import Effectful.Reader.Dynamic (Reader)
import Text.Read (readMaybe)
import Web.View


-- | A 'Component' is a self-contained piece of a 'Page'.
class (ViewId c, IsAction (Action c)) => Component c es where
  -- | The data used by the component.
  data Model c


  -- | The actions supported by the component.
  data Action c


  -- | Additional effects required by the component. Can be omitted.
  type Effects c (es :: [Effect]) :: Constraint


  type Effects c es = ()


  -- | Other components nested in the component. Can be omitted.
  type Require c :: [Type]


  type Require c = '[]


  -- | Render the component.
  render :: Model c -> View c ()


  -- | Update the component based on a message. Ideally would only change the data model and leave rendering to the 'render' function.
  update :: (Effects c es) => Action c -> Eff (Reader c : es) (View c ())


class ViewId a where
  toViewId :: a -> Text
  default toViewId :: (Show a) => a -> Text
  toViewId = pack . show


  parseViewId :: Text -> Maybe a
  default parseViewId :: (Read a) => Text -> Maybe a
  parseViewId = readMaybe . unpack


class (Show a, Read a) => IsAction a where
  toAction :: a -> Text
  default toAction :: (Show a) => a -> Text
  toAction = pack . show


  parseAction :: Text -> Maybe a
  default parseAction :: (Read a) => Text -> Maybe a
  parseAction = readMaybe . unpack


instance IsAction () where
  toAction _ = ""
  parseAction _ = Just ()
