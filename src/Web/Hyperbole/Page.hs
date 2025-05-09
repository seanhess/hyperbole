module Web.Hyperbole.Page where

import Data.Kind (Type)
import Effectful
import Web.Hyperbole.Effect.Handler (RunHandlers, runLoad)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server (Response)
import Web.Hyperbole.HyperView (Root)
import Web.Hyperbole.View (View)


{- | Conceptually, an application is dividied up into multiple [Pages](#g:pages). Each page module should have a function that returns a 'Page'. The 'Page' itself is a 'View' with a type-level list of 'HyperView's used on the page.

@
#EMBED Example/Docs/MultiView.hs page
@
-}
type Page (views :: [Type]) = View (Root views) ()


{- | Run a 'Page' and return a 'Response'

@
#EMBED Example/Docs/BasicPage.hs main

#EMBED Example/Docs/BasicPage.hs page
@
-}
runPage
  :: (Hyperbole :> es, RunHandlers views es)
  => Eff es (Page views)
  -> Eff es Response
runPage = runLoad
