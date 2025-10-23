{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.HyperView.Hyper where

import Web.Atomic.Types
import Web.Hyperbole.Data.Encoded as Encoded
import Web.Hyperbole.HyperView.Handled (HyperViewHandled)
import Web.Hyperbole.HyperView.Types
import Web.Hyperbole.HyperView.ViewId
import Web.Hyperbole.View (View, addContext, tag)


{- | Embed a 'HyperView' into a page or another 'View'

@
#EMBED Example/Docs/Interactive.hs page
@
-}
hyper
  :: forall id ctx
   . (HyperViewHandled id ctx, ViewId id, ConcurrencyValue (Concurrency id))
  => id
  -> View id ()
  -> View ctx ()
hyper = hyperUnsafe


hyperUnsafe :: forall id ctx. (ViewId id, ConcurrencyValue (Concurrency id)) => id -> View id () -> View ctx ()
hyperUnsafe vid vw = do
  tag "div" @ att "id" (encodedToText $ toViewId vid) . concurrency $
    addContext vid vw
 where
  concurrency =
    case concurrencyMode @(Concurrency id) of
      Drop -> id
      Replace -> att "data-concurrency" (encode Replace)
