{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView.Types where

import Data.Kind (Type)
import Effectful
import Effectful.Reader.Dynamic
import Effectful.State.Dynamic
import GHC.Generics
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.Effect.Hyperbole (Hyperbole)
import Web.Hyperbole.View (View (..), ViewAction, ViewId (..), none)


-- HyperView --------------------------------------------

{- | HyperViews are interactive subsections of a 'Page'

Create an instance with a unique view id type and a sum type describing the actions the HyperView supports. The View Id can contain context (a database id, for example)

@
#EMBED Example.Simple data Message

#EMBED Example.Simple instance HyperView Message es where
@
-}
class (ViewId id, ViewAction (Action id), ConcurrencyValue (Concurrency id)) => HyperView id es where
  -- | Outline all actions that are permitted in this HyperView
  --
  -- > data Action Message = SetMessage Text | ClearMessage
  -- >   deriving (Generic, ViewAction)
  data Action id


  -- | Include any child hyperviews here. The compiler will make sure that the page knows how to handle them
  --
  -- > type Require Messages = '[ChildView]
  type Require id :: [Type]


  type Require id = '[]


  -- type ViewState id :: Type
  -- type ViewState id = ()

  -- | Control how overlapping actions are handled. 'Drop' by default
  --
  -- > type Concurrency Autocomplete = Replace
  type Concurrency id :: ConcurrencyMode


  type Concurrency id = Drop


  -- | Specify how the view should be updated for each Action
  --
  -- > update (SetMessage msg) = pure $ messageView msg
  -- > update ClearMessage = pure $ messageView ""
  update :: (Hyperbole :> es) => Action id -> Eff (Reader id : State (ViewState id) : es) (View id ())


instance HyperView () es where
  data Action () = TupleNone
    deriving (Generic, ViewAction)
  update _ = pure none


-- convert the type to a value
class ConcurrencyValue a where
  concurrencyMode :: ConcurrencyMode
instance ConcurrencyValue 'Drop where
  concurrencyMode = Drop
instance ConcurrencyValue 'Replace where
  concurrencyMode = Replace


data ConcurrencyMode
  = -- | Do not send any actions that occur while one is active. Prevents double-submitting writes or expensive operations
    Drop
  | -- | Ignore the results of older actions in favor of new ones. Use for read-only views with fast-firing interactions, like autocomplete, sliders, etc
    Replace
  deriving (Generic, ToEncoded, FromEncoded)


-- | The top-level view returned by a 'Page'. It carries a type-level list of every 'HyperView' used in our 'Page' so the compiler can check our work and wire everything together.
data Root (views :: [Type]) = Root
  deriving (Generic, ViewId)


instance HyperView (Root views) es where
  data Action (Root views) = RootNone
    deriving (Generic, ViewAction)
  type Require (Root views) = views
  update _ = pure none
