{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.HyperView where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Kind (Type)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import GHC.Generics
import Text.Read (readMaybe)
import Web.Hyperbole.Param (GParam (..), Param (..), breakSegment)
import Web.Hyperbole.Route (Route (..), routeUrl)
import Web.View
import Data.ByteString qualified as BS
import Debug.Trace
import Data.ByteString.Lazy qualified as BL


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
class (ViewId id, ViewAction (Action id)) => HyperView id where
  type Action id :: Type


class ViewAction a where
  toAction :: a -> Text
  default toAction :: (Generic a, GAction (Rep a)) => a -> Text
  toAction = gToAction . from


  parseAction :: Text -> Maybe a
  default parseAction :: (Generic a, GAction (Rep a)) => Text -> Maybe a
  parseAction t = to <$> gParseAction t


class ViewId a where
  toViewId :: a -> Text
  default toViewId :: (Generic a, GParam (Rep a)) => a -> Text
  toViewId = gToParam . from


  parseViewId :: Text -> Maybe a
  default parseViewId :: (Generic a, GParam (Rep a)) => Text -> Maybe a
  parseViewId t = to <$> gParseParam t


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
  el (att "id" (toViewId vid) . flexCol) $
    addContext vid vw


{- | \<button\> HTML tag which sends the action when pressed

> button SomeAction (border 1) "Click Me"
-}
button :: (HyperView id) => Action id -> Mod -> View id () -> View id ()
button a f cd = do
  c <- context
  tag "button" (att "data-on-click" (toAction a) . dataTarget c . f) cd


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
  el (att "data-on-load" (toAction a) . att "data-delay" (toParam delay) . dataTarget c) initContent


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
dataTarget :: (ViewId a) => a -> Mod
dataTarget = att "data-target" . toViewId


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
dropdown act isSel f options = do
  c <- context
  tag "select" (att "data-on-change" "" . dataTarget c . f) $ do
    addContext (Option act isSel) options


-- | An option for a 'dropdown'. First argument is passed to (opt -> Action id) in the 'dropdown', and to the selected predicate
option
  :: (HyperView id, Eq opt)
  => opt
  -> View (Option opt id (Action id)) ()
  -> View (Option opt id (Action id)) ()
option opt cnt = do
  os <- context
  tag "option" (att "value" (toAction (os.toAction opt)) . selected (os.selected opt)) cnt


-- | sets selected = true if the 'dropdown' predicate returns True
selected :: Bool -> Mod
selected b = if b then att "selected" "true" else id


-- | The view context for an 'option'
data Option opt id action = Option
  { toAction :: opt -> action
  , selected :: opt -> Bool
  }


{- | A hyperlink to another route

>>> route (User 100) id "View User"
<a href="/user/100">View User</a>
-}
route :: (Route a) => a -> Mod -> View c () -> View c ()
route r = link (routeUrl r)


class GAction f where
  gToAction :: f p -> Text
  gParseAction :: Text -> Maybe (f p)


instance (GAction f, GAction g) => GAction (f :*: g) where
  gToAction (a :*: b) = gToAction a <> " " <> gToAction b

  gParseAction t = do
    let (at, bt) = breakProduct
    a <- gParseAction at
    b <- gParseAction bt
    pure $ a :*: b

    where
      breakProduct
        -- if it starts with a ( or a ", we want to skip until the terminator
        | T.isPrefixOf "\"" t = breakString
        | T.isPrefixOf "(" t = breakParens
        | otherwise = breakSegment ' ' t

      breakString =
        let rest = T.drop 1 t
        in ("\"" <> T.takeWhile (/= '"') rest <> "\"", T.drop 2 $ T.dropWhile (/= '"') rest)

      breakParens =
        let rest = T.drop 1 t
        in ("(" <> T.takeWhile (/= ')') rest <> ")", T.drop 2 $ T.dropWhile (/= ')') rest)



instance (GAction f, GAction g) => GAction (f :+: g) where
  gToAction (L1 a) = gToAction a
  gToAction (R1 b) = gToAction b
  gParseAction t = do
    (L1 <$> gParseAction @f t) <|> (R1 <$> gParseAction @g t)


instance (Datatype d, GAction f) => GAction (M1 D d f) where
  gToAction (M1 a) = gToAction a
  gParseAction t = M1 <$> gParseAction t


instance (Constructor c, GAction f) => GAction (M1 C c f) where
  gToAction (M1 a) =
    let cn = conName (undefined :: M1 C c f p)
     in case gToAction a of
          "" -> pack cn
          t -> pack cn <> " " <> t
  gParseAction t = do
    let (c, rest) = breakSegment ' ' t
    guard $ c == pack (conName (undefined :: M1 C c f p))
    M1 <$> gParseAction rest


instance GAction U1 where
  gToAction _ = ""
  gParseAction _ = pure U1


instance (GAction f) => GAction (M1 S s f) where
  gToAction (M1 a) = gToAction a
  gParseAction t = M1 <$> gParseAction t


instance GAction (K1 R Int) where
  gToAction = toActionShow
  gParseAction = parseActionRead

instance GAction (K1 R Integer) where
  gToAction = toActionShow
  gParseAction = parseActionRead

instance GAction (K1 R String) where
  gToAction = toActionShow
  gParseAction = parseActionRead

instance GAction (K1 R Text) where
  gToAction = toActionShow
  gParseAction = parseActionRead

instance GAction (K1 R BS.ByteString) where
  gToAction = toActionShow
  gParseAction = parseActionRead

instance GAction (K1 R BL.ByteString) where
  gToAction = toActionShow
  gParseAction = parseActionRead




instance {-# OVERLAPPABLE #-} (Show a, Read a) => GAction (K1 R a) where
  gToAction (K1 a) = "(" <> pack (show a) <> ")"
  gParseAction t = K1 <$> readMaybe (unpack t)


toActionShow :: Show a => K1 R a p -> Text
toActionShow (K1 a) = pack (show a)

parseActionRead :: Read a => Text -> Maybe (K1 R a p)
parseActionRead t = K1 <$> readMaybe (unpack t)
