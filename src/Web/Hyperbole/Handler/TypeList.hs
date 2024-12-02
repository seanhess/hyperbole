{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Handler.TypeList where

import Data.Kind (Constraint, Type)
import GHC.TypeLits hiding (Mod)


-- type family AllDescendents (xs :: [Type]) :: [Type] where
--   AllDescendents xs = xs <++> RemoveAll xs (NextDescendents '[] xs)

-- concat lists
type family (<++>) xs ys where
  '[] <++> ys = ys
  xs <++> '[] = xs
  (x ': xs) <++> ys = x : xs <++> ys


type family Remove x ys where
  Remove x '[] = '[]
  Remove x (x ': ys) = Remove x ys
  Remove x (y ': ys) = y ': Remove x ys


type family RemoveAll xs ys where
  RemoveAll '[] ys = ys
  RemoveAll xs '[] = '[]
  RemoveAll (x ': xs) ys = RemoveAll xs (Remove x ys)


-- Type family to check if an element is in a type-level list
type Elem e es = ElemOr e es (NotElem e es)


-- 'orig' is used to store original list for better error messages
type family ElemOr e es err :: Constraint where
  ElemOr x (x ': xs) err = ()
  ElemOr y (x ': xs) err = ElemOr y xs err
  -- Note [Custom Errors]
  ElemOr x '[] err = err


type family AllElemOr xs ys err :: Constraint where
  AllElemOr '[] _ _ = ()
  AllElemOr (x ': xs) ys err =
    (ElemOr x ys err, AllElemOr xs ys err)


type NotElem x (orig :: [Type]) =
  TypeError
    ( 'ShowType x
        ':<>: 'Text " not found in "
        ':<>: 'ShowType orig
    )
