{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Handler where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Network.HTTP.Types (queryToQueryText)
import Web.Hyperbole.Effect.Event (getEvent, lookupEvent)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request (request)
import Web.Hyperbole.Effect.Respond (respondEarly)
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.HyperView
import Web.View


class RunHandlers (views :: [Type]) es where
  runHandlers :: (Hyperbole :> es) => Eff es ()


instance RunHandlers '[] es where
  runHandlers = pure ()


instance (HyperView view es, RunHandlers views es) => RunHandlers (view : views) es where
  runHandlers = do
    runHandler @view (update @view)
    runHandlers @views


runHandler
  :: forall id es
   . (HyperView id es, Hyperbole :> es)
  => (Action id -> Eff (Reader id : es) (View id ()))
  -> Eff es ()
runHandler run = do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- getEvent @id :: Eff es (Maybe (Event id (Action id)))
  case mev of
    Just event -> do
      vw <- runReader event.viewId $ run event.action
      respondEarly event.viewId vw
    _ -> do
      pure ()


runLoad
  :: forall views es
   . (Hyperbole :> es, RunHandlers views es)
  => Eff es (View (Root views) ())
  -> Eff es Response
runLoad loadPage = do
  runHandlers @views
  guardNoEvent
  loadToResponse loadPage


guardNoEvent :: (Hyperbole :> es) => Eff es ()
guardNoEvent = do
  r <- request
  case lookupEvent (queryToQueryText r.query) of
    -- Are id and action set to something?
    Just e -> send $ RespondEarly $ Err $ ErrNotHandled e
    Nothing -> pure ()


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
