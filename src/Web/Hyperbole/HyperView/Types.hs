{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Web.Hyperbole.HyperView.Types where

import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Effectful
import Effectful.Reader.Dynamic
import GHC.Generics
import GHC.TypeLits hiding (Mod)
import Web.Atomic.CSS (flexCol, (~))
import Web.Atomic.Types (att, (@))
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.Effect.Hyperbole (Hyperbole)
import Web.Hyperbole.TypeList
import Web.Hyperbole.View (View, addContext, context, none, tag)


{- | HyperViews are interactive subsections of a 'Page'

Create an instance with a unique view id type and a sum type describing the actions the HyperView supports. The View Id can contain context (a database id, for example)

@
#EMBED Example/Docs/Interactive.hs data Message

#EMBED Example/Docs/Interactive.hs instance HyperView Message es where
@
-}
class (ViewId id, ViewAction (Action id)) => HyperView id es where
  -- | Outline all actions that are permitted in this HyperView
  --
  -- > data Action Message = SetMessage Text | ClearMessage
  -- >   deriving (Generic, ViewAction)
  data Action id


  -- | Include any child hyperviews here. The compiler will make sure that the page knows how to handle them
  --
  -- > type Require = '[ChildView]
  type Require id :: [Type]


  type Require id = '[]


  -- | Specify how the view should be updated for each Action
  --
  -- > update (SetMessage msg) = pure $ messageView msg
  -- > update ClearMessage = pure $ messageView ""
  update :: (Hyperbole :> es) => Action id -> Eff (Reader id : es) (View id ())


-- | The top-level view returned by a 'Page'. It carries a type-level list of every 'HyperView' used in our 'Page' so the compiler can check our work and wire everything together.
data Root (views :: [Type]) = Root
  deriving (Generic, ViewId)


instance HyperView (Root views) es where
  data Action (Root views) = RootNone
    deriving (Generic, ViewAction)
  type Require (Root views) = views
  update _ = pure none


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
    ElemOr id (Require ctx) (NotHandled id ctx (Require ctx))
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
  tag "div" @ att "id" (toViewId vid) ~ flexCol $
    addContext vid vw


class ViewAction a where
  toAction :: a -> Text
  default toAction :: (Generic a, GToEncoded (Rep a)) => a -> Text
  toAction = genericEncode


  parseAction :: Text -> Maybe a
  default parseAction :: (Generic a, GFromEncoded (Rep a)) => Text -> Maybe a
  parseAction = genericDecode


instance ViewAction () where
  toAction _ = ""
  parseAction _ = Just ()


class ViewId a where
  toViewId :: a -> Text
  default toViewId :: (Generic a, GToEncoded (Rep a)) => a -> Text
  toViewId = genericEncode


  parseViewId :: Text -> Maybe a
  default parseViewId :: (Generic a, GFromEncoded (Rep a)) => Text -> Maybe a
  parseViewId = genericDecode


{- | Access the 'viewId' in a 'View' or 'update'

@
#EMBED Example/Page/LazyLoading.hs data LazyData

#EMBED Example/Page/LazyLoading.hs instance (Debug :> es, GenRandom :> es) => HyperView LazyData es where
@
-}
class HasViewId m view where
  viewId :: m view


instance HasViewId (View ctx) ctx where
  viewId = context
instance HasViewId (Eff (Reader view : es)) view where
  viewId = ask
