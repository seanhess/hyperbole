{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView.Hyper where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as A
import Data.String.Conversions (cs)
import Web.Atomic.Types
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.HyperView.Handled (HyperViewHandled)
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.View (View, runViewContext, tag)
import Web.Hyperbole.View.ViewId


{- | Embed a 'HyperView' into a page or another 'View'

@
#EMBED Example.Docs.Interactive page
@
-}
hyper
  :: forall id ctx
   . (HyperViewHandled id ctx, ViewId id, ViewState id ~ (), ConcurrencyValue (Concurrency id))
  => id
  -> View id ()
  -> View ctx ()
hyper vid = hyperState vid ()


hyperState
  :: forall id ctx
   . (HyperViewHandled id ctx, ViewId id, ToJSON (ViewState id), ConcurrencyValue (Concurrency id))
  => id
  -> ViewState id
  -> View id ()
  -> View ctx ()
hyperState = hyperUnsafe


hyperUnsafe :: forall id ctx. (ViewId id, ViewState id ~ ViewState id, ToJSON (ViewState id), ConcurrencyValue (Concurrency id)) => id -> ViewState id -> View id () -> View ctx ()
hyperUnsafe vid st vw = do
  tag "div" @ att "id" (encodedToText $ toViewId vid) . state . concurrency $
    runViewContext vid st vw
 where
  concurrency =
    case concurrencyMode @(Concurrency id) of
      Drop -> id
      Replace -> att "data-concurrency" (encode Replace)

  state =
    let enc = A.encode st
     in if enc == mempty
          then id
          else att "data-state" (cs enc)
