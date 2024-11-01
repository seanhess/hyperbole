{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView.Class where

import Data.Kind (Type)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Web.Hyperbole.Page (Root (..))


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


instance HyperView (Root views) where
  type Action (Root views) = ()
  type Require (Root views) = views


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


instance ViewId (Root views)
