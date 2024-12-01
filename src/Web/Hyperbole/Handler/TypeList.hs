{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Handler.TypeList where

import Data.Kind (Constraint, Type)
import GHC.TypeLits hiding (Mod)
import Web.Hyperbole.Component (Component (..))
import Web.Hyperbole.HyperView


-- type family AllDescendents (xs :: [Type]) :: [Type] where
--   AllDescendents xs = xs <++> RemoveAll xs (NextDescendents '[] xs)

type family ValidDescendents x :: [Type] where
  ValidDescendents x = x : NextDescendents '[] '[x]


type family NextDescendents (ex :: [Type]) (xs :: [Type]) where
  NextDescendents _ '[] = '[]
  NextDescendents ex (x ': xs) =
    RemoveAll (x : ex) (Import x)
      <++> NextDescendents ((x : ex) <++> Import x) (RemoveAll (x : ex) (Import x))
      <++> NextDescendents (x : ex) (RemoveAll (x : ex) xs)


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


type NotHandled id ctx (views :: [Type]) =
  TypeError
    ( 'Text "HyperView "
        :<>: 'ShowType id
        :<>: 'Text " not found in (Import "
        :<>: 'ShowType ctx
        :<>: 'Text ")"
        :$$: 'Text "  " :<>: 'ShowType views
        :$$: 'Text "Try adding it to the HyperView instance:"
        :$$: 'Text "  instance HyperView " :<>: 'ShowType ctx :<>: 'Text " where"
        :$$: 'Text "    type Action " :<>: 'ShowType ctx :<>: 'Text " = " :<>: ShowType (Msg id) :<>: 'Text ""
        :$$: 'Text "    type Import " :<>: 'ShowType ctx :<>: 'Text " = [" :<>: ShowType id :<>: 'Text ", ...]"
    )


type NotDesc id ctx x cs =
  TypeError
    ( 'Text ""
        :<>: 'ShowType x
        :<>: 'Text ", a child of HyperView "
        :<>: 'ShowType id
        :<>: 'Text ", not handled by context "
        :<>: 'ShowType ctx
        :$$: ('Text " Import = " ':<>: 'ShowType cs)
        -- ':$$: 'ShowType x
        -- ':$$: 'ShowType cs
    )


type NotInPage x total =
  TypeError
    ( 'Text ""
        :<>: 'ShowType x
        :<>: 'Text " not included in: "
        :$$: 'Text "  Page es " :<>: ShowType total
        :$$: 'Text "try expanding the page views to:"
        :$$: 'Text "  Page es " :<>: ShowType (x : total)
        -- :$$: 'Text " " :<>: 'ShowType ctx :<>: 'Text " = " :<>: ShowType (Action id) :<>: 'Text ""
        -- :$$: 'Text "    page :: (Hyperbole :> es) => Page es '[" :<>: 'ShowType ctx :<>: 'Text " = [" :<>: ShowType id :<>: 'Text ", ...]"
    )


type HyperViewHandled id ctx es =
  ( Component id es
  , Component ctx es
  , -- the id must be found in the children of the context
    ElemOr id (Import ctx) (NotHandled id ctx (Import ctx))
  , -- Make sure the descendents of id are in the context for the root page
    CheckDescendents id ctx
  )


-- TODO: Report which view requires the missing one
type family CheckDescendents id ctx :: Constraint where
  CheckDescendents id (Root total) =
    ( AllInPage (ValidDescendents id) total
    )
  CheckDescendents id ctx = ()


type family AllInPage ids total :: Constraint where
  AllInPage '[] _ = ()
  AllInPage (x ': xs) total =
    (ElemOr x total (NotInPage x total), AllInPage xs total)


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
