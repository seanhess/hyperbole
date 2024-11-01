module Web.Hyperbole.Page where

import Data.Kind (Type)
import Effectful (Eff, Effect)
import Web.View (View)


{- | Hyperbole applications are divided into Pages. Each Page must 'load' the whole page , and 'handle' each /type/ of 'HyperView'

@
myPage :: ('Hyperbole' :> es) => 'Page' es 'Response'
myPage = do
  'handle' messages
  'load' pageView

pageView = do
  el_ "My Page"
  'hyper' (Message 1) $ messageView "Starting Message"
@
-}
newtype Page (es :: [Effect]) (views :: [Type]) = Page (Eff es (View (Root views) ()))


-- | The top-level view created by 'load'. Carries the views in its type to check that we handled all our views
data Root (views :: [Type]) = Root
  deriving (Show, Read)
