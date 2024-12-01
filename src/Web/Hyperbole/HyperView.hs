{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView where

import Data.Kind (Type)
import Web.Hyperbole.Component (Component (..), IsAction, ViewId)


-- | The top-level view created by 'load'. Carries the views in its type to check that we handled all our views
data Root (views :: [Type]) = Root
  deriving (Show, Read, ViewId)


instance Component (Root views) es where
  data Action (Root views) = RootAction
    deriving (Show, Read, IsAction)


  data Model (Root views) = RootModel
    deriving (Show, Read)


  type Require (Root views) = views
  render _ = pure ()
  update _ = undefined -- TODO: Never called? How to best handle this?


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
