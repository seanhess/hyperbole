{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Handler where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Web.Hyperbole.Component
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.HyperView
import Web.Hyperbole.View.Target (hyperUnsafe)
import Web.View


class HasViewId m view where
  viewId :: m view
instance HasViewId (View ctx) ctx where
  viewId = context
instance HasViewId (Eff (Reader view : es)) view where
  viewId = ask


-- -- If the actions are the same (newtype), map the views and have the inner handler process it
-- delegate
--   :: forall view inner es
--    . (Msg view ~ Msg inner, Component inner (Reader view : es ), Hyperbole :> es)
--   => (view -> inner)
--   -> Msg view
--   -> Eff (Reader view : es) (View view ())
-- delegate f action = do
--   c <- viewId
--   let inner = f c
--   innerView <- runReader inner $ update @inner @(_) action
--   pure $ addContext inner innerView

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


instance (Effects view es, Component view es, Read (Msg view), RunHandlers views es, ViewId view) => RunHandlers (view : views) es where
  runHandlers = do
    runHandler @view (update @view @es)
    runHandlers @views


-- handle
--   :: forall views es
--    . (Handle (TupleList views), Hyperbole :> es)
--   => Handlers es (TupleList views)
--   -> Eff es (View (Root (TupleList views)) ())
--   -> Page es views
-- handle handlers loadPage = Page $ do
--   runHandlers @(TupleList views) handlers
--   guardNoEvent
--   loadPage

runHandler
  :: forall id es
   . (Component id es, Read (Msg id), ViewId id, Hyperbole :> es)
  => (Msg id -> Eff (Reader id : es) (View id ()))
  -> Eff es ()
runHandler run = do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- getEvent @id :: Eff es (Maybe (Event id (Msg id)))
  case mev of
    Just event -> do
      vw <- runReader event.viewId $ run event.action
      let vid = TargetViewId $ toViewId event.viewId
      send $ RespondEarly $ Response vid $ hyperUnsafe @id @es event.viewId vw
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
