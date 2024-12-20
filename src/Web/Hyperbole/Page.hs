module Web.Hyperbole.Page where

import Data.Kind (Type)
import Effectful
import Web.Hyperbole.Effect.Handler (RunHandlers, runLoad)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server (Response)
import Web.Hyperbole.HyperView (Root)
import Web.View (View)


-- type Page (es :: [Effect]) (views :: [Type]) = Eff es (View (Root views) ())
type Page (views :: [Type]) = View (Root views) ()


-- | Run a 'Page' in 'Hyperbole'
runPage
  :: forall views es
   . (Hyperbole :> es, RunHandlers views es)
  => Eff es (Page views)
  -> Eff es Response
runPage = runLoad
