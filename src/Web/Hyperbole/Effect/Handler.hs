{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Effect.Handler where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Effect.Event (getEvent)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Request (request)
import Web.Hyperbole.Effect.Response (respondView)
import Web.Hyperbole.Effect.Server
import Web.Hyperbole.HyperView
import Web.Hyperbole.View


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
    Just evt -> do
      vw <- runReader evt.viewId $ run evt.action
      respondView evt.viewId vw
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
  q <- (.query) <$> request
  case lookupEvent q of
    -- Are id and action set to something?
    Just e -> send $ RespondNow $ Err $ ErrNotHandled e
    Nothing -> pure ()


loadToResponse :: Eff es (View (Root total) ()) -> Eff es Response
loadToResponse run = do
  vw <- run
  let vid = TargetViewId (encodedToText $ toViewId Root)
  let res = Response vid $ addContext Root vw
  pure res
