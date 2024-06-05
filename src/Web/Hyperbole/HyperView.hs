{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.HyperView where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Kind (Type)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import GHC.Generics
import Text.Read
import Web.Hyperbole.Route (Route (..), routeUrl)
import Web.View


{- | HyperViews are interactive subsections of a 'Page'

Create an instance with a unique view id type and a sum type describing the actions the HyperView supports. The View Id can contain context (a database id, for example)

@
data Message = Message Int
  deriving (Generic, 'Param')

data MessageAction
  = Louder Text
  | ClearMessage
  deriving (Generic, 'Param')

instance HyperView Message where
  type Action Message = MessageAction
@
-}
class (Param id, Param (Action id)) => HyperView id where
  type Action id :: Type
  hyperViewMod :: id -> Mod
  default hyperViewMod :: id -> Mod
  hyperViewMod _ = id



{- | Embed HyperViews into the page, or nest them into other views

@
myPage :: ('Hyperbole' :> es) => 'Page' es 'Response'
myPage = do
  'handle' messages
  'load' $ do
    pure $ do
      'el_' "My Page"
      'hyper' (Message 1) $ messageView "Hello World"
      'hyper' (Message 2) $ do
        messageView "Another Message"
        'hyper' OtherView otherView
@

Views can only trigger actions that match their HyperView

@
messageView :: Text -> View Message ()
messageView m = do
  el_ (text m)
  button (Louder m) "Louder"

otherView :: View OtherView ()
otherView = do
  -- Type Error!
  button (Louder \"Hi\") id "Louder"
@
-}
hyper :: forall id ctx. (HyperView id) => id -> View id () -> View ctx ()
hyper vid vw = do
  el (att "id" (toParam vid) . hyperViewMod vid) $
    addContext vid vw


{- | \<button\> HTML tag which sends the action when pressed

> button SomeAction (border 1) "Click Me"
-}
button :: (HyperView id) => Action id -> Mod -> View id () -> View id ()
button a f cd = do
  c <- context
  tag "button" (att "data-on-click" (toParam a) . dataTarget c . f) cd


{- | Send the action after N milliseconds. Can be used to implement lazy loading or polling

@
pollMessageView :: Text -> 'View' Message ()
pollMessageView m = do
  onLoad LoadMessage 1000 $ do
    'el' 'bold' "Current Message. Reloading in 1s"
    'el_' ('text' m)
@
-}
onLoad :: (HyperView id) => Action id -> DelayMs -> View id () -> View id ()
onLoad a delay initContent = do
  c <- context
  el (att "data-on-load" (toParam a) . att "data-delay" (toParam delay) . dataTarget c) initContent


type DelayMs = Int


{- | Give visual feedback when an action is in-flight.

@
myView = do
  onRequest loadingIndicator $ do
    'el_' \"Loaded\"
  where
    loadingIndicator = 'el_' "Loading..."
@
-}
onRequest :: View id () -> View id () -> View id ()
onRequest a b = do
  el (parent "hyp-loading" flexCol . hide) a
  el (parent "hyp-loading" hide . flexCol) b


-- | Internal
dataTarget :: (Param a) => a -> Mod
dataTarget = att "data-target" . toParam


{- | Trigger actions for another view. They will update the view specified

> otherView :: View OtherView ()
> otherView = do
>   el_ "This is not a message view"
>   button OtherAction id "Do Something"
>
>   target (Message 2) $ do
>     el_ "Now we can trigger a MessageAction which will update our Message HyperView, not this one"
>     button ClearMessage id "Clear Message #2"
-}
target :: (HyperView id) => id -> View id () -> View a ()
target = addContext


{- | Type-safe dropdown. Sends (opt -> Action id) when selected. The selection predicate (opt -> Bool) controls which option is selected. See [Example.Contacts](https://github.com/seanhess/hyperbole/blob/main/example/Example/Contacts.hs)

@
data ContactsAction
  = Reload (Maybe Filter)
  | Delete Int
  deriving (Generic, Param)

allContactsView :: Maybe Filter -> View Contacts ()
allContactsView fil = do
  row (gap 10) $ do
    el (pad 10) "Filter: "
    dropdown Reload (== fil) id $ do
      option Nothing ""
      option (Just Active) "Active!"
      option (Just Inactive) "Inactive"
  ...
@
-}
dropdown
  :: (HyperView id)
  => (opt -> Action id)
  -> (opt -> Bool) -- check if selec
  -> Mod
  -> View (Option opt id (Action id)) ()
  -> View id ()
dropdown toAction isSel f options = do
  c <- context
  tag "select" (att "data-on-change" "" . dataTarget c . f) $ do
    addContext (Option toAction isSel) options


-- | An option for a 'dropdown'. First argument is passed to (opt -> Action id) in the 'dropdown', and to the selected predicate
option
  :: (HyperView id, Eq opt)
  => opt
  -> View (Option opt id (Action id)) ()
  -> View (Option opt id (Action id)) ()
option opt cnt = do
  os <- context
  tag "option" (att "value" (toParam (os.toAction opt)) . selected (os.selected opt)) cnt


-- | sets selected = true if the 'dropdown' predicate returns True
selected :: Bool -> Mod
selected b = if b then att "selected" "true" else id


-- | The view context for an 'option'
data Option opt id action = Option
  { toAction :: opt -> action
  , selected :: opt -> Bool
  }


{- | Types that can be serialized. 'HyperView' requires this for both its view id and action

> data Message = Message Int
>   deriving (Generic, Param)
-}
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


{- | A hyperlink to another route

>>> route (User 100) id "View User"
<a href="/user/100">View User</a>
-}
route :: (Route a) => a -> Mod -> View c () -> View c ()
route r = link (routeUrl r)
