{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Handler where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.HyperView
import Web.Hyperbole.View.Target (hyperUnsafe)
import Web.View


class Handle view es where
  handle :: (Hyperbole :> es) => Action view -> Eff (Reader view : es) (View view ())


class HasViewId m view where
  viewId :: m view
instance HasViewId (View ctx) ctx where
  viewId = context
instance HasViewId (Eff (Reader view : es)) view where
  viewId = ask


type Handler es view a = Eff (Reader view : es) a


-- If the actions are the same (newtype), map the views and have the inner handler process it
delegate
  :: forall view inner es
   . (Action view ~ Action inner, Handle inner (Reader view : es), Hyperbole :> es)
  => (view -> inner)
  -> Action view
  -> Eff (Reader view : es) (View view ())
delegate f action = do
  c <- viewId
  let inner = f c
  innerView <- runReader inner $ handle @inner action
  pure $ addContext inner innerView


mapView :: (view -> inner) -> View inner () -> View view ()
mapView f inner = do
  view1 <- viewId
  addContext (f view1) inner


-- class SetContext (f :: Type -> Type -> Type) es cnew cold where
--   setViewId :: cnew -> (f es) cnew () -> (f es) cold ()
-- instance SetContext (View Identity) a b where
--   setViewId = addContext
-- instance (f es view ~ Eff (Reader view : es), Handle view es) => SetContext (f es) cnew cold where
--   setViewId :: cnew -> (f es) cnew () -> (f es) cold ()
--   setViewId cnew action = runReader cnew action

class RunHandlers (views :: [Type]) es where
  runHandlers :: (Hyperbole :> es) => Eff es ()


instance RunHandlers '[] es where
  runHandlers = pure ()


instance (HyperView view, Handle view es, RunHandlers views es) => RunHandlers (view : views) es where
  runHandlers = do
    runHandler @view (handle @view)
    runHandlers @views


instance (HyperView a, HyperView b, HyperView c, HyperView d, HyperView e, HyperView f, HyperView g) => Handle '[a, b, c, d, e, f, g] where
  type Handlers es '[a, b, c, d, e, f, g] = (Handlers es '[a], Handlers es '[b], Handlers es '[c], Handlers es '[d], Handlers es '[e], Handlers es '[f], Handlers es '[g])
  runHandlers (a, b, c, d, e, f, g) = do
    runHandlers @'[a, b, c, d, e, f] (a, b, c, d, e, f)
    runHandler g


instance (HyperView a, HyperView b, HyperView c, HyperView d, HyperView e, HyperView f, HyperView g, HyperView h) => Handle '[a, b, c, d, e, f, g, h] where
  type Handlers es '[a, b, c, d, e, f, g, h] = (Handlers es '[a], Handlers es '[b], Handlers es '[c], Handlers es '[d], Handlers es '[e], Handlers es '[f], Handlers es '[g], Handlers es '[h])
  runHandlers (a, b, c, d, e, f, g, h) = do
    runHandlers @'[a, b, c, d, e, f, g] (a, b, c, d, e, f, g)
    runHandler h


instance (HyperView a, HyperView b, HyperView c, HyperView d, HyperView e, HyperView f, HyperView g, HyperView h, HyperView i) => Handle '[a, b, c, d, e, f, g, h, i] where
  type Handlers es '[a, b, c, d, e, f, g, h, i] = (Handlers es '[a], Handlers es '[b], Handlers es '[c], Handlers es '[d], Handlers es '[e], Handlers es '[f], Handlers es '[g], Handlers es '[h], Handlers es '[i])
  runHandlers (a, b, c, d, e, f, g, h, i) = do
    runHandlers @'[a, b, c, d, e, f, g, h] (a, b, c, d, e, f, g, h)
    runHandler i


instance (HyperView a, HyperView b, HyperView c, HyperView d, HyperView e, HyperView f, HyperView g, HyperView h, HyperView i, HyperView j) => Handle '[a, b, c, d, e, f, g, h, i, j] where
  type Handlers es '[a, b, c, d, e, f, g, h, i, j] = (Handlers es '[a], Handlers es '[b], Handlers es '[c], Handlers es '[d], Handlers es '[e], Handlers es '[f], Handlers es '[g], Handlers es '[h], Handlers es '[i], Handlers es '[j])
  runHandlers (a, b, c, d, e, f, g, h, i, j) = do
    runHandlers @'[a, b, c, d, e, f, g, h, i] (a, b, c, d, e, f, g, h, i)
    runHandler j


handle
  :: forall views es
   . (Handle (TupleList views), Hyperbole :> es, HyperViewHandler view es)
  => Handlers es (TupleList views)
  -> Eff es (View (Root (TupleList views)) ())
  -> Page es views
handle handlers loadPage = Page $ do
  runHandlers @(TupleList views) handlers
  guardNoEvent
  loadPage


runHandler
  :: forall id es
   . (HyperView id, Hyperbole :> es)
  => (Action id -> Eff (Reader id : es) (View id ()))
  -> Eff es ()
runHandler run = do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- getEvent @id :: Eff es (Maybe (Event id (Action id)))
  case mev of
    Just event -> do
      vw <- runReader event.viewId $ run event.action
      let vid = TargetViewId $ toViewId event.viewId
      send $ RespondEarly $ Response vid $ hyperUnsafe event.viewId vw
    _ -> do
      pure ()


guardNoEvent :: (Hyperbole :> es) => Eff es ()
guardNoEvent = do
  r <- request
  case lookupEvent r.query of
    -- Are id and action set to something?
    Just e -> send $ RespondEarly $ Err $ ErrNotHandled e
    Nothing -> pure ()


runLoad
  :: forall views es
   . (Hyperbole :> es, RunHandlers views es)
  => Eff es (View (Root views) ())
  -> Eff es Response
runLoad loadPage = do
  runHandlers @views
  guardNoEvent
  loadToResponse loadPage


loadToResponse :: Eff es (View (Root total) ()) -> Eff es Response
loadToResponse run = do
  vw <- run
  let vid = TargetViewId (toViewId Root)
  let res = Response vid $ addContext Root vw
  pure res


-- deriving newtype (Applicative, Monad, Functor)

{- | The load handler is run when the page is first loaded. Run any side effects needed, then return a view of the full page

@
myPage :: (Hyperbole :> es) => UserId -> Page es Response
myPage userId = do
  'load' $ do
    user <- loadUserFromDatabase userId
    pure $ userPageView user
@
-}

-- load
--   :: (Hyperbole :> es)
--   => Eff es (View (Root views) ())
--   -> Page views es Response
-- load run = Page $ do
--   r <- request
--   case lookupEvent r.query of
--     -- Are id and action set to sometjhing?
--     Just e ->
--       pure $ Err $ ErrNotHandled e
--     Nothing -> do
--       vw <- run
--       view vw

{- | A handler is run when an action for that 'HyperView' is triggered. Run any side effects needed, then return a view of the corresponding type

@
myPage :: ('Hyperbole' :> es) => 'Page' es 'Response'
myPage = do
  'handle' messages
  'load' pageView

messages :: ('Hyperbole' :> es, MessageDatabase) => Message -> MessageAction -> 'Eff' es ('View' Message ())
messages (Message mid) ClearMessage = do
  deleteMessageSideEffect mid
  pure $ messageView ""

messages (Message mid) (Louder m) = do
  let new = m <> "!"
  saveMessageSideEffect mid new
  pure $ messageView new
@
-}

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

-- pageView :: (Hyperbole :> es, Handlers views es) => View (Root views) () -> Eff es (Page views)
-- pageView = pure
