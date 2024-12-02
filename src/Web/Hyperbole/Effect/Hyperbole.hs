{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Hyperbole where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Kind (Constraint, Type)
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.String.Conversions
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Dynamic
import Effectful.State.Static.Local
import GHC.TypeLits hiding (Mod)
import Web.FormUrlEncoded (Form, urlDecodeForm)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData (..), parseQueryParam)
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.Handler.TypeList
import Web.Hyperbole.HyperView
import Web.Hyperbole.Route
import Web.Hyperbole.Session as Session
import Web.View


{- | In any 'load' or 'handle', you can use this Effect to get extra request information or control the response manually.

For most 'Page's, you won't need to use this effect directly. Use custom 'Route's for request info, and return 'View's to respond
-}
data Hyperbole :: Effect where
  GetRequest :: Hyperbole m Request
  RespondEarly :: Response -> Hyperbole m a
  SetSession :: (ToHttpApiData a) => Text -> a -> Hyperbole m ()
  DelSession :: Text -> Hyperbole m ()
  GetSession :: (FromHttpApiData a) => Text -> Hyperbole m (Maybe a)


type instance DispatchOf Hyperbole = 'Dynamic


-- | Run the 'Hyperbole' effect to 'Server'
runHyperbole
  :: (Server :> es)
  => Eff (Hyperbole : es) Response
  -> Eff es Response
runHyperbole = fmap combine $ reinterpret runLocal $ \_ -> \case
  GetRequest -> do
    gets @HyperState (.request)
  RespondEarly r -> do
    s <- gets @HyperState (.session)
    send $ SendResponse s r
    throwError_ r
  SetSession k a -> do
    modify $ \st -> st{session = sessionSet k a st.session} :: HyperState
  DelSession k -> do
    modify $ \st -> st{session = sessionDel k st.session} :: HyperState
  GetSession k -> do
    s <- gets @HyperState (.session)
    pure $ sessionLookup k s
 where
  runLocal :: (Server :> es) => Eff (State HyperState : Error Response : es) a -> Eff es (Either Response (a, HyperState))
  runLocal eff = do
    -- Load the request ONCE right when we start
    r <- send LoadRequest
    let st = HyperState r (sessionFromCookies r.cookies)
    runErrorNoCallStack @Response . runState st $ eff

  combine :: (Server :> es) => Eff es (Either Response (Response, HyperState)) -> Eff es Response
  combine eff = do
    er <- eff
    case er of
      Left res ->
        -- responded early, don't need to respond again
        pure res
      Right (res, st) -> do
        send $ SendResponse st.session res
        pure res


data HyperState = HyperState
  { request :: Request
  , session :: Session
  }


-- | Return all information about the 'Request'
request :: (Hyperbole :> es) => Eff es Request
request = send GetRequest


{- | Return the request path

>>> reqPath
["users", "100"]
-}
reqPath :: (Hyperbole :> es) => Eff es [Segment]
reqPath = (.path) <$> request


{- | Return the request body as a Web.FormUrlEncoded.Form

Prefer using Type-Safe 'Form's when possible
-}
formBody :: (Hyperbole :> es) => Eff es Form
formBody = do
  b <- (.body) <$> request
  let ef = urlDecodeForm b
  -- not going to work. we need a way to `throwError` or it doesn't work...
  either (send . RespondEarly . Err . ErrParse) pure ef


getEvent :: (HyperView id es, Hyperbole :> es) => Eff es (Maybe (Event id (Action id)))
getEvent = do
  q <- reqParams
  pure $ do
    Event ti ta <- lookupEvent q
    vid <- parseViewId ti
    act <- parseAction ta
    pure $ Event vid act


lookupEvent :: Query -> Maybe (Event Text Text)
lookupEvent q' =
  Event
    <$> lookupParam "id" q'
    <*> lookupParam "action" q'


{- | Lookup a session variable by keyword

> load $ do
>   tok <- session "token"
>   ...
-}
session :: (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es (Maybe a)
session k = send $ GetSession k


{- | Set a session variable by keyword

> load $ do
>   t <- reqParam "token"
>   setSession "token" t
>   ...
-}
setSession :: (Hyperbole :> es, ToHttpApiData a) => Text -> a -> Eff es ()
setSession k v = send $ SetSession k v


-- | Clear the user's session
clearSession :: (Hyperbole :> es) => Text -> Eff es ()
clearSession k = send $ DelSession k


{- | Return the entire 'Query'

@
myPage :: 'Page' es 'Response'
myPage = do
  'load' $ do
    q <- reqParams
    case 'lookupParam' "token" q of
      Nothing -> pure $ errorView "Missing Token in Query String"
      Just t -> do
        sideEffectUsingToken token
        pure myPageView
@
-}
reqParams :: (Hyperbole :> es) => Eff es Query
reqParams = (.query) <$> request


{- | Require a given parameter from the 'Query' arguments

@
myPage :: 'Page' es 'Response'
myPage = do
  'load' $ do
    token <- reqParam "token"
    sideEffectUsingToken token
    pure myPageView
@
-}
reqParam :: forall a es. (Hyperbole :> es, FromHttpApiData a) => Text -> Eff es a
reqParam p = do
  q <- reqParams
  (er :: Either Response a) <- pure $ do
    mv <- require $ List.lookup (encodeUtf8 p) q
    v <- require mv
    first (Err . ErrParam) $ parseQueryParam (decodeUtf8 v)
  case er of
    Left e -> send $ RespondEarly e
    Right a -> pure a
 where
  require :: Maybe x -> Either Response x
  require Nothing = Left $ Err $ ErrParam $ "Missing: " <> p
  require (Just a) = pure a


-- | Lookup the query param in the 'Query'
lookupParam :: BS.ByteString -> Query -> Maybe Text
lookupParam p q =
  fmap cs <$> join $ lookup p q


-- | Whether the param is present or not
hasParam :: BS.ByteString -> Query -> Bool
hasParam p q =
  isJust $ lookup p q


{- | Respond immediately with 404 Not Found

@
userLoad :: (Hyperbole :> es, Users :> es) => UserId -> Eff es User
userLoad uid = do
  mu <- send (LoadUser uid)
  maybe notFound pure mu

myPage :: (Hyperbole :> es, Users :> es) => Eff es View
myPage = do
  load $ do
    u <- userLoad 100
    -- skipped if user = Nothing
    pure $ userView u
@
-}
notFound :: (Hyperbole :> es) => Eff es a
notFound = send $ RespondEarly NotFound


-- | Respond immediately with a parse error
parseError :: (Hyperbole :> es) => Text -> Eff es a
parseError = send . RespondEarly . Err . ErrParse


-- | Redirect immediately to the 'Url'
redirect :: (Hyperbole :> es) => Url -> Eff es a
redirect = send . RespondEarly . Redirect


-- | Respond with the given view, and stop execution
respondEarly :: (Hyperbole :> es, HyperView id es) => id -> View id () -> Eff es ()
respondEarly i vw = do
  let vid = TargetViewId (toViewId i)
  let res = Response vid $ hyperUnsafe i vw
  send $ RespondEarly res


-- | Manually set the response to the given view. Normally you return a 'View' from 'load' or 'handle' instead of using this
view :: (Hyperbole :> es) => View () () -> Eff es Response
view vw = do
  pure $ Response (TargetViewId "") vw


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
class (ViewId id, ViewAction (Action id)) => HyperView id es where
  data Action id


  type Require id :: [Type]
  type Require id = '[]


  handle :: (Hyperbole :> es) => Action id -> Eff (Reader id : es) (View id ())


-- | The top-level view created by 'load'. Carries the views in its type to check that we handled all our views
data Root (views :: [Type]) = Root
  deriving (Show, Read, ViewId)


instance HyperView (Root views) es where
  data Action (Root views) = RootNone
    deriving (Show, Read, ViewAction)
  type Require (Root views) = views
  handle _ = pure none


type family ValidDescendents x :: [Type] where
  ValidDescendents x = x : NextDescendents '[] '[x]


type family NextDescendents (ex :: [Type]) (xs :: [Type]) where
  NextDescendents _ '[] = '[]
  NextDescendents ex (x ': xs) =
    RemoveAll (x : ex) (Require x)
      <++> NextDescendents ((x : ex) <++> Require x) (RemoveAll (x : ex) (Require x))
      <++> NextDescendents (x : ex) (RemoveAll (x : ex) xs)


type NotHandled id ctx (views :: [Type]) =
  TypeError
    ( 'Text "HyperView "
        :<>: 'ShowType id
        :<>: 'Text " not found in (Require "
        :<>: 'ShowType ctx
        :<>: 'Text ")"
        :$$: 'Text "  "
          :<>: 'ShowType views
        :$$: 'Text "Try adding it to the HyperView instance:"
        :$$: 'Text "  instance HyperView "
          :<>: 'ShowType ctx
          :<>: 'Text " where"
        :$$: 'Text "    type Action "
          :<>: 'ShowType ctx
          :<>: 'Text " = "
          :<>: ShowType (Action id)
          :<>: 'Text ""
        :$$: 'Text "    type Require "
          :<>: 'ShowType ctx
          :<>: 'Text " = ["
          :<>: ShowType id
          :<>: 'Text ", ...]"
    )


type NotDesc id ctx x cs =
  TypeError
    ( 'Text ""
        :<>: 'ShowType x
        :<>: 'Text ", a child of HyperView "
        :<>: 'ShowType id
        :<>: 'Text ", not handled by context "
        :<>: 'ShowType ctx
        :$$: ('Text " Require = " ':<>: 'ShowType cs)
        -- ':$$: 'ShowType x
        -- ':$$: 'ShowType cs
    )


type NotInPage x total =
  TypeError
    ( 'Text ""
        :<>: 'ShowType x
        :<>: 'Text " not included in: "
        :$$: 'Text "  Page es "
          :<>: ShowType total
        :$$: 'Text "try expanding the page views to:"
        :$$: 'Text "  Page es "
          :<>: ShowType (x : total)
          -- :$$: 'Text " " :<>: 'ShowType ctx :<>: 'Text " = " :<>: ShowType (Action id) :<>: 'Text ""
          -- :$$: 'Text "    page :: (Hyperbole :> es) => Page es '[" :<>: 'ShowType ctx :<>: 'Text " = [" :<>: ShowType id :<>: 'Text ", ...]"
    )


type HyperViewHandled id ctx =
  ( -- the id must be found in the children of the context
    ElemOr id (Require ctx) (NotHandled id ctx (Require ctx))
  , -- Make sure the descendents of id are in the context for the root page
    CheckDescendents id ctx
  )


-- TODO: Report which view requires the missing one
type family CheckDescendents id ctx :: Constraint where
  CheckDescendents id (Root total) =
    ( AllInPage (ValidDescendents id) total
    )
  CheckDescendents id ctx = ()


type family AllInPage ids total :: Constraint where
  AllInPage '[] _ = ()
  AllInPage (x ': xs) total =
    (ElemOr x total (NotInPage x total), AllInPage xs total)


-- TODO: if I'm going to limit it, it's going to happen here
-- AND all their children have to be there
-- , All (Elem (Require ctx)) (Require id)
hyper
  :: forall id ctx
   . (HyperViewHandled id ctx, ViewId id)
  => id
  -> View id ()
  -> View ctx ()
hyper = hyperUnsafe


hyperUnsafe :: (ViewId id) => id -> View id () -> View ctx ()
hyperUnsafe vid vw = do
  el (att "id" (toViewId vid) . flexCol) $
    addContext vid vw


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
target :: (HyperViewHandled id ctx) => id -> View id () -> View ctx ()
target = addContext
