{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView where

import Data.Kind (Type)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)


{- | HyperViews are interactive subsections of a 'Page'

Create an instance with a unique view id type and a sum type describing the actions the HyperView supports. The View Id can contain context (a database id, for example)

@
data Message = Message Int
  deriving (Generic, 'Param')

data MessageAction
  = Louder Text
  | ClearMessage
  deriving (Generic, 'Param')

instance HyperView Message where
  type Action Message = MessageAction
@
-}
class (ViewId id, ViewAction (Action id)) => HyperView id where
  type Action id :: Type
  type Require id :: [Type]
  type Require id = '[]


class ViewAction a where
  toAction :: a -> Text
  default toAction :: (Show a) => a -> Text
  toAction = pack . show


  parseAction :: Text -> Maybe a
  default parseAction :: (Read a) => Text -> Maybe a
  parseAction = readMaybe . unpack


instance ViewAction () where
  toAction _ = ""
  parseAction _ = Just ()


class ViewId a where
  toViewId :: a -> Text
  default toViewId :: (Show a) => a -> Text
  toViewId = pack . show


  parseViewId :: Text -> Maybe a
  default parseViewId :: (Read a) => Text -> Maybe a
  parseViewId = readMaybe . unpack


-- | The top-level view created by 'load'. Carries the views in its type to check that we handled all our views
data Root (views :: [Type]) = Root
  deriving (Show, Read, ViewId)


instance HyperView (Root views) where
  type Action (Root views) = ()
  type Require (Root views) = views


type family TupleList a where
  TupleList () = '[]
  TupleList (a, b) = [a, b]
  TupleList (a, b, c) = [a, b, c]
  TupleList (a, b, c, d) = [a, b, c, d]
  TupleList (a, b, c, d, e) = [a, b, c, d, e]
  TupleList (a, b, c, d, e, f) = [a, b, c, d, e, f]
  TupleList (a, b, c, d, e, f, g) = [a, b, c, d, e, f, g]
  TupleList (a, b, c, d, e, f, g, h) = [a, b, c, d, e, f, g, h]
  TupleList (a, b, c, d, e, f, g, h, i) = [a, b, c, d, e, f, g, h, i]
  TupleList (a, b, c, d, e, f, g, h, i, j) = [a, b, c, d, e, f, g, h, i, j]
  TupleList a = '[a]
