{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.Handler
  ( Page (..)
  , page
  , handle
  , Hyperbole (..)
  )
where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.HyperView
import Web.Hyperbole.View.Target (hyperUnsafe)
import Web.View


class Handle (views :: [Type]) where
  type Handlers (es :: [Effect]) views :: Type
  runHandlers :: (Hyperbole :> es) => Handlers es views -> Eff es ()


instance Handle '[] where
  type Handlers es '[] = ()
  runHandlers _ = pure ()


instance (HyperView id) => Handle '[id] where
  type Handlers es '[id] = id -> Action id -> Eff es (View id ())
  runHandlers action = do
    runHandler action


instance (HyperView a, HyperView b) => Handle '[a, b] where
  type Handlers es '[a, b] = (Handlers es '[a], Handlers es '[b])
  runHandlers (a, b) = do
    runHandler a
    runHandlers @'[b] b


instance (HyperView a, HyperView b, HyperView c) => Handle '[a, b, c] where
  type Handlers es '[a, b, c] = (Handlers es '[a], Handlers es '[b], Handlers es '[c])
  runHandlers (a, b, c) = do
    runHandlers @'[a, b] (a, b)
    runHandler c


instance (HyperView a, HyperView b, HyperView c, HyperView d) => Handle '[a, b, c, d] where
  type Handlers es '[a, b, c, d] = (Handlers es '[a], Handlers es '[b], Handlers es '[c], Handlers es '[d])
  runHandlers (a, b, c, d) = do
    runHandlers @'[a, b, c] (a, b, c)
    runHandler d


instance (HyperView a, HyperView b, HyperView c, HyperView d, HyperView e) => Handle '[a, b, c, d, e] where
  type Handlers es '[a, b, c, d, e] = (Handlers es '[a], Handlers es '[b], Handlers es '[c], Handlers es '[d], Handlers es '[e])
  runHandlers (a, b, c, d, e) = do
    runHandlers @'[a, b, c, d] (a, b, c, d)
    runHandler e


instance (HyperView a, HyperView b, HyperView c, HyperView d, HyperView e, HyperView f) => Handle '[a, b, c, d, e, f] where
  type Handlers es '[a, b, c, d, e, f] = (Handlers es '[a], Handlers es '[b], Handlers es '[c], Handlers es '[d], Handlers es '[e], Handlers es '[f])
  runHandlers (a, b, c, d, e, f) = do
    runHandlers @'[a, b, c, d, e] (a, b, c, d, e)
    runHandler f


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
   . (Handle (TupleList views), Hyperbole :> es)
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
  => (id -> Action id -> Eff es (View id ()))
  -> Eff es ()
runHandler run = do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- getEvent @id :: Eff es (Maybe (Event id (Action id)))
  case mev of
    Just event -> do
      vw <- run event.viewId event.action
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
newtype Page (es :: [Effect]) (views :: Type) = Page (Eff es (View (Root (TupleList views)) ()))


-- | Run a 'Page' in 'Hyperbole'
page
  :: forall views es
   . (Hyperbole :> es)
  => Page es views
  -> Eff es Response
page (Page run) = do
  vw <- run
  let vid = TargetViewId (toViewId Root)
  let res = Response vid $ addContext Root vw
  pure res
