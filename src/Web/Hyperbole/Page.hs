module Web.Hyperbole.Page where

import Data.Kind (Type)
import Effectful
import Effectful.Reader.Dynamic
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.HyperView (Root (..))
import Web.Hyperbole.Server.Handler (RunHandlers, runLoad)
import Web.Hyperbole.Types.Response (Response)
import Web.Hyperbole.View (View)


{- | Conceptually, an application is dividied up into multiple [Pages](#g:pages). Each page module should have a function that returns a 'Page'. The 'Page' itself is a 'View' with a type-level list of 'HyperView's used on the page.

@
#EMBED Example/Docs/MultiView.hs page
@
-}

type Page es (views :: [Type]) = Eff (Reader (Root views) : es) (View (Root views) ())


{- | Run a 'Page' and return a 'Response'

@
#EMBED Example/Docs/BasicPage.hs main

#EMBED Example/Docs/BasicPage.hs page
@
-}
runPage
  :: (Hyperbole :> es, RunHandlers views es)
  => Page es views
  -> Eff es Response
runPage eff = runLoad $ runReader Root eff


subPage
  :: (Hyperbole :> es)
  => Eff (Reader (Root inner) : es) a
  -> Eff es a
subPage pg = do
  runReader Root pg
