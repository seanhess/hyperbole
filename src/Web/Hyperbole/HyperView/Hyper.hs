{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView.Hyper where

import Web.Atomic.Types
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.HyperView.Handled (HyperViewHandled)
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.View (View, runViewContext, tag)
import Web.Hyperbole.View.ViewId


{- | Embed a 'HyperView' into a page or another 'View'

@
#EMBED Example/Docs/Interactive.hs page
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
   . (HyperViewHandled id ctx, ViewId id, ToEncoded (ViewState id), ConcurrencyValue (Concurrency id))
  => id
  -> ViewState id
  -> View id ()
  -> View ctx ()
hyperState = hyperUnsafe


hyperUnsafe :: forall id ctx. (ViewId id, ViewState id ~ ViewState id, ToEncoded (ViewState id), ConcurrencyValue (Concurrency id)) => id -> ViewState id -> View id () -> View ctx ()
hyperUnsafe vid st vw = do
  tag "div" @ att "id" (encodedToText $ toViewId vid) . state . concurrency $
    runViewContext vid st vw
 where
  concurrency =
    case concurrencyMode @(Concurrency id) of
      Drop -> id
      Replace -> att "data-concurrency" (encode Replace)

  state =
    if encode st == mempty
      then id
      else att "data-state" (encode st)
