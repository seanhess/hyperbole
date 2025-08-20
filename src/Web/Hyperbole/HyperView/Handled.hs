{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView.Handled where

import Data.Kind (Constraint, Type)
import GHC.TypeLits hiding (Mod)
import Web.Atomic.Types
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.HyperView.ViewId
import Web.Hyperbole.TypeList
import Web.Hyperbole.View (View, addContext, tag)


{- | Embed a 'HyperView' into another 'View'

@
#EMBED Example/Docs/Interactive.hs page
@
-}
hyper
  :: forall id ctx
   . (HyperViewHandled id ctx, ViewId id)
  => id
  -> View id ()
  -> View ctx ()
hyper = hyperUnsafe


hyperUnsafe :: (ViewId id) => id -> View id () -> View ctx ()
hyperUnsafe vid vw = do
  tag "div" @ att "id" (encodedToText $ toViewId vid) $
    addContext vid vw


type family ValidDescendents x :: [Type] where
  ValidDescendents x = x : NextDescendents '[] '[x]


type family NextDescendents (ex :: [Type]) (xs :: [Type]) where
  NextDescendents _ '[] = '[]
  NextDescendents ex (x ': xs) =
    RemoveAll (x : ex) (Require x)
      <++> NextDescendents ((x : ex) <++> Require x) (RemoveAll (x : ex) (Require x))
      <++> NextDescendents (x : ex) (RemoveAll (x : ex) xs)


type NotHandled id ctx (views :: [Type]) =
  TypeError
    ( 'Text "HyperView "
        :<>: 'ShowType id
        :<>: 'Text " not found in (Require "
        :<>: 'ShowType ctx
        :<>: 'Text ")"
        :$$: 'Text "  "
          :<>: 'ShowType views
        :$$: 'Text "Try adding it to the HyperView instance:"
        :$$: 'Text "  instance HyperView "
          :<>: 'ShowType ctx
          :<>: 'Text " where"
        :$$: 'Text "    type Action "
          :<>: 'ShowType ctx
          :<>: 'Text " = "
          :<>: ShowType (Action id)
          :<>: 'Text ""
        :$$: 'Text "    type Require "
          :<>: 'ShowType ctx
          :<>: 'Text " = ["
          :<>: ShowType id
          :<>: 'Text ", ...]"
    )


type NotDesc id ctx x cs =
  TypeError
    ( 'Text ""
        :<>: 'ShowType x
        :<>: 'Text ", a child of HyperView "
        :<>: 'ShowType id
        :<>: 'Text ", not handled by context "
        :<>: 'ShowType ctx
        :$$: ('Text " Require = " ':<>: 'ShowType cs)
        -- ':$$: 'ShowType x
        -- ':$$: 'ShowType cs
    )


type NotInPage x total =
  TypeError
    ( 'Text ""
        :<>: 'ShowType x
        :<>: 'Text " not included in: "
        :$$: 'Text "  Page es "
          :<>: ShowType total
        :$$: 'Text "try expanding the page views to:"
        :$$: 'Text "  Page es "
          :<>: ShowType (x : total)
          -- :$$: 'Text " " :<>: 'ShowType ctx :<>: 'Text " = " :<>: ShowType (Action id) :<>: 'Text ""
          -- :$$: 'Text "    page :: (Hyperbole :> es) => Page es '[" :<>: 'ShowType ctx :<>: 'Text " = [" :<>: ShowType id :<>: 'Text ", ...]"
    )


type HyperViewHandled id ctx =
  ( -- the id must be found in the children of the context
    ElemOr id (ctx : Require ctx) (NotHandled id ctx (Require ctx))
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
