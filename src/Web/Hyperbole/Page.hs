module Web.Hyperbole.Page where

import Data.Kind (Type)
import Effectful
import Effectful.Reader.Dynamic
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.HyperView (Root (..))
import Web.Hyperbole.Server.Handler (RunHandlers, runLoad)
import Web.Hyperbole.Types.Response (Response)
import Web.Hyperbole.View (View)


{- ! An application is divided into multiple [Pages](#g:pages). Each page module should have a 'Page' function, which returns a root 'View'

@
#EMBED Example.Docs.MultiView page
@
-}

{- | An application is divided into multiple [Pages](#g:pages). Each page module should have a 'Page' function, which returns a root 'View'

@
page :: 'Page' es [Message, Counter]
page = do
  pure $ do
    'hyper' Message1 $ messageView \"Hello\"
    'hyper' Message2 $ messageView \"World\"
    'hyper' Counter $ viewCount 0
@
-}
type Page es (views :: [Type]) = Eff (Reader (Root views) : es) (View (Root views) ())


{- ! Run a 'Page' and return a 'Response'

@
#EMBED Example.Docs.BasicPage main

#EMBED Example.Docs.BasicPage page
@
-}

{- | Run a 'Page' and return a 'Response'

@
main :: IO ()
main = do
  'run' 3000 $ 'liveApp' 'quickStartDocument' ('runPage' page)

page :: 'Page' es '[]
page = do
  pure $ 'el' \"Hello World\"
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
