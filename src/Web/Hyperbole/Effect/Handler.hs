{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Handler where

import Data.Kind (Type)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Page (Page)
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.HyperView
import Web.Hyperbole.Types.Event
import Web.Hyperbole.View


class RunHandlers (views :: [Type]) es where
  runHandlers :: (Page :> es) => Event TargetViewId Text -> Eff (Hyperbole : es) (Maybe (View () ()))


instance RunHandlers '[] es where
  runHandlers _ = pure Nothing


instance (HyperView view (Hyperbole : es), RunHandlers views es) => RunHandlers (view : views) es where
  runHandlers rawEvent = do
    mview <- runHandler @view rawEvent (update @view)

    case mview of
      Nothing -> runHandlers @views rawEvent
      Just vw -> pure $ Just vw


runHandler
  :: forall id es
   . (HyperView id es)
  => Event TargetViewId Text
  -> (Action id -> Eff (Reader id : es) (View id ()))
  -> Eff es (Maybe (View () ()))
runHandler rawEvent update_ = do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- decodeHyperEvent @id rawEvent :: Eff es (Maybe (Event id (Action id)))
  case mev of
    Just evt -> do
      vw <- runReader evt.viewId $ update_ evt.action
      pure $ Just $ addContext evt.viewId vw
    _ -> do
      pure Nothing

--
--
-- runLoad
--   :: forall views es
--    . (Hyperbole :> es, RunHandlers views es)
--   => Eff es (View (Root views) ())
--   -> Eff es Response
-- runLoad loadPage = do
--   runHandlers @views
--   guardNoEvent
--   loadToResponse loadPage
--
--
-- guardNoEvent :: (Hyperbole :> es) => Eff es ()
-- guardNoEvent = do
--   q <- (.query) <$> request
--   case lookupEvent q of
--     -- Are id and action set to something?
--     Just e -> send $ RespondNow $ Err $ ErrNotHandled e
--     Nothing -> pure ()
--
--
-- loadToResponse :: Eff es (View (Root total) ()) -> Eff es Response
-- loadToResponse run = do
--   vw <- run
--   let vid = TargetViewId (encodedToText $ toViewId Root)
--   let res = Response vid $ addContext Root vw
--   pure res
