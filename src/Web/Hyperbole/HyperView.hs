{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

module Web.Hyperbole.HyperView where

import Data.Kind (Type)
import Data.Text
import Text.Read
import Web.Hyperbole.Route (Route (..), pathUrl)
import Web.View


-- | Associate a live id with a set of actions
class (Param id, Param (Action id)) => HyperView id where
  type Action id :: Type


viewId :: forall id ctx. (HyperView id) => id -> View id () -> View ctx ()
viewId vid vw = do
  el (att "id" (toParam vid) . flexCol) $
    addContext vid vw


button :: (HyperView id) => Action id -> Mod -> View id () -> View id ()
button a f cd = do
  c <- context
  tag "button" (att "data-on-click" (toParam a) . dataTarget c . f) cd


onRequest :: View id () -> View id () -> View id ()
onRequest a b = do
  el (parent "hyp-loading" flexCol . hide) a
  el (parent "hyp-loading" hide . flexCol) b


type DelayMs = Int


onLoad :: (HyperView id) => Action id -> DelayMs -> View id () -> View id ()
onLoad a delay initContent = do
  c <- context
  el (att "data-on-load" (toParam a) . att "data-delay" (toParam delay) . dataTarget c) initContent


-- | Internal
dataTarget :: (Param a) => a -> Mod
dataTarget = att "data-target" . toParam


-- | Change the target of any code running inside, allowing actions to target other live views on the page
target :: (HyperView id) => id -> View id () -> View a ()
target = addContext


dropdown
  :: (HyperView id)
  => (opt -> action)
  -> (opt -> Bool)
  -> Mod
  -> View (Option opt id action) ()
  -> View id ()
dropdown toAction isSel f options = do
  c <- context
  tag "select" (att "data-on-change" "" . dataTarget c . f) $ do
    addContext (Option toAction isSel) options


option
  :: (HyperView id, Eq opt)
  => opt
  -> View (Option opt id (Action id)) ()
  -> View (Option opt id (Action id)) ()
option opt cnt = do
  os <- context
  tag "option" (att "value" (toParam (os.toAction opt)) . selected (os.selected opt)) cnt


selected :: Bool -> Mod
selected b = if b then att "selected" "true" else id


data Option opt id action = Option
  { toAction :: opt -> action
  , selected :: opt -> Bool
  }


class Param a where
  -- not as flexible as FromHttpApiData, but derivable
  parseParam :: Text -> Maybe a
  default parseParam :: (Read a) => Text -> Maybe a
  parseParam = readMaybe . unpack


  toParam :: a -> Text
  default toParam :: (Show a) => a -> Text
  toParam = pack . show


instance Param Integer
instance Param Float
instance Param Int
instance Param ()


instance Param Text where
  parseParam = pure
  toParam = id


link :: (Route a) => a -> Mod -> View c () -> View c ()
link r f cnt = do
  let Url u = pathUrl . routePath $ r
  tag "a" (att "href" u . f) cnt
