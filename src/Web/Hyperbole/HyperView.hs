{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.HyperView where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Kind (Type)
import Data.String.Conversions (cs)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Debug.Trace
import GHC.Generics
import Text.Casing (kebab)
import Text.Read
import Web.Hyperbole.Route (Route (..), routeUrl)
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
  toParam :: a -> Text
  default toParam :: (Generic a, GParam (Rep a)) => a -> Text
  toParam = gToParam . from


  -- not as flexible as FromHttpApiData, but derivable
  parseParam :: Text -> Maybe a
  default parseParam :: (Generic a, GParam (Rep a)) => Text -> Maybe a
  parseParam t = to <$> gParseParam t


class GParam f where
  gToParam :: f p -> Text
  gParseParam :: Text -> Maybe (f p)


instance (GParam f, GParam g) => GParam (f :*: g) where
  gToParam (a :*: b) = gToParam a <> "-" <> gToParam b
  gParseParam t = do
    let (at, bt) = breakSegment t
    a <- gParseParam at
    b <- gParseParam bt
    pure $ a :*: b


instance (GParam f, GParam g) => GParam (f :+: g) where
  gToParam (L1 a) = gToParam a
  gToParam (R1 b) = gToParam b
  gParseParam t = do
    (L1 <$> gParseParam @f t) <|> (R1 <$> gParseParam @g t)


-- do we add the datatypename? no, the constructor name
instance (Datatype d, GParam f) => GParam (M1 D d f) where
  gToParam (M1 a) = gToParam a
  gParseParam t = M1 <$> gParseParam t


instance (Constructor c, GParam f) => GParam (M1 C c f) where
  gToParam (M1 a) =
    let cn = toSegment (conName (undefined :: M1 C c f p))
     in case gToParam a of
          "" -> cn
          t -> cn <> "-" <> t
  gParseParam t = do
    let (c, rest) = breakSegment t
    guard $ c == toSegment (conName (undefined :: M1 C c f p))
    M1 <$> gParseParam rest


instance GParam U1 where
  gToParam _ = ""
  gParseParam _ = pure U1


instance (GParam f) => GParam (M1 S s f) where
  gToParam (M1 a) = gToParam a
  gParseParam t = M1 <$> gParseParam t


instance GParam (K1 R Text) where
  gToParam (K1 t) = t
  gParseParam t = pure $ K1 t


instance GParam (K1 R String) where
  gToParam (K1 s) = pack s
  gParseParam t = pure $ K1 $ unpack t


instance {-# OVERLAPPABLE #-} (Param a) => GParam (K1 R a) where
  gToParam (K1 a) = toParam a
  gParseParam t = K1 <$> parseParam t


-- instance {-# OVERLAPPABLE #-} (Show a, Read a) => GParam (K1 R a) where
--   gToParam (K1 a) = pack $ show a
--   gParseParam t = do
--     K1 <$> readMaybe (unpack t)

breakSegment :: Text -> (Text, Text)
breakSegment t =
  let (start, rest) = T.breakOn "-" t
   in (start, T.drop 1 rest)


toSegment :: String -> Text
toSegment = T.toLower . pack


-- instance (GParam f) => GParam (M1 C c f) where
--   gForm = M1 gForm

-- where
--  toDouble '\'' = '\"'
--  toDouble c = c

instance (Param a) => Param (Maybe a) where
  toParam Nothing = ""
  toParam (Just a) = toParam a
  parseParam "" = pure Nothing
  parseParam t = Just $ parseParam t
instance Param Integer where
  toParam = pack . show
  parseParam = readMaybe . unpack
instance Param Float where
  toParam = pack . show
  parseParam = readMaybe . unpack
instance Param Int where
  toParam = pack . show
  parseParam = readMaybe . unpack
instance Param () where
  toParam = pack . show
  parseParam = readMaybe . unpack


instance Param Text where
  parseParam = pure
  toParam = id


-- | Link to another route
route :: (Route a) => a -> Mod -> View c () -> View c ()
route r = link (routeUrl r)


data Boot = Boot Int Text deriving (Generic)


test :: Boot
test = to $ from $ Boot 3 "hello"
