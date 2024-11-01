{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Web.Hyperbole.Handler
  ( Page (..)
  , load
  , handle
  , page
  , Hyperbole (..)
  , Handler
  )
where

import Data.Kind (Constraint, Type)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request
import Web.Hyperbole.Effect.Response
import Web.Hyperbole.HyperView
import Web.Hyperbole.Page
import Web.Hyperbole.View.Target (hyperUnsafe)
import Web.View


data Handler (view :: Type) :: Effect where
  RespondEvents :: Handler view m ()


type instance DispatchOf (Handler view) = Dynamic


type family Handlers (views :: [Type]) (es :: [Effect]) :: Constraint where
  Handlers '[] es = ()
  Handlers (x ': xs) es = (Handler x :> es, Handlers xs es)


load :: (Hyperbole :> es, Handlers total es) => Eff es (View (Root total) ()) -> Page es total
load run = Page $ do
  r <- request
  case lookupEvent r.query of
    -- Are id and action set to something?
    Just e -> send $ RespondEarly $ Err $ ErrNotHandled e
    Nothing -> run


loadToResponse :: Eff es (View (Root total) ()) -> Eff es Response
loadToResponse run = do
  vw <- run
  let vid = TargetViewId (toViewId Root)
  let res = Response vid $ addContext Root vw
  pure res


-- but we actually have to run the handler here...
-- this IS the handler running
handle
  :: forall id total es
   . (HyperView id, Hyperbole :> es)
  => (id -> Action id -> Eff es (View id ()))
  -> Page (Handler id : es) total
  -> Page es total
handle action (Page inner) = Page $ do
  runHandler action $ do
    send $ RespondEvents @id
    inner


runHandler
  :: forall id es a
   . (HyperView id, Hyperbole :> es)
  => (id -> Action id -> Eff es (View id ()))
  -> Eff (Handler id : es) a
  -> Eff es a
runHandler run = interpret $ \_ -> \case
  RespondEvents -> do
    -- Get an event matching our type. If it doesn't match, skip to the next handler
    mev <- getEvent @id :: Eff es (Maybe (Event id (Action id)))
    case mev of
      Just event -> do
        vw <- run event.viewId event.action
        let vid = TargetViewId $ toViewId event.viewId
        send $ RespondEarly $ Response vid $ hyperUnsafe event.viewId vw
      _ -> do
        pure ()


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

-- runHandler
--   :: forall id es
--    . (Hyperbole :> es, HyperView id)
--   => (id -> Action id -> Eff es (View id ()))
--   -> Eff es ()
-- runHandler run = do
--   -- Get an event matching our type. If it doesn't match, skip to the next handler
--   mev <- getEvent @id
--   case mev of
--     Just event -> do
--       vw <- run event.viewId event.action
--       let vid = TargetViewId $ toViewId event.viewId
--       send $ RespondEarly $ Response vid $ hyperUnsafe event.viewId vw
--     _ -> pure ()

-- | Run a 'Page' in 'Hyperbole'
page
  :: forall views es
   . (Hyperbole :> es)
  => Page es views
  -> Eff es Response
page (Page eff) = do
  loadToResponse eff
