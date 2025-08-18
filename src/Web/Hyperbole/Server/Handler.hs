{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Hyperbole.Server.Handler where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Web.Hyperbole.Data.Encoded
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.Response (hyperView, respondError)
import Web.Hyperbole.HyperView
import Web.Hyperbole.Types.Event
import Web.Hyperbole.Types.Request
import Web.Hyperbole.Types.Response
import Web.Hyperbole.View


class RunHandlers (views :: [Type]) es where
  runHandlers :: (Hyperbole :> es) => Event TargetViewId Encoded -> Eff es (Maybe Response)


instance RunHandlers '[] es where
  runHandlers _ = pure Nothing


instance (HyperView view es, RunHandlers views es) => RunHandlers (view : views) es where
  runHandlers rawEvent = do
    mr <- runHandler @view rawEvent (update @view)
    case mr of
      Nothing -> runHandlers @views rawEvent
      Just r -> pure (Just r)


runHandler
  :: forall id es
   . (HyperView id es, Hyperbole :> es)
  => Event TargetViewId Encoded
  -> (Action id -> Eff (Reader id : es) (View id ()))
  -> Eff es (Maybe Response)
runHandler rawEvent run = do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- decodeEvent @id rawEvent :: Eff es (Maybe (Event id (Action id)))
  case mev of
    Just evt -> do
      vw <- runReader evt.viewId $ run evt.action
      res <- hyperView evt.viewId vw
      pure $ Just res
    _ -> do
      pure Nothing


runLoad
  :: forall views es
   . (Hyperbole :> es, RunHandlers views es)
  => Eff es (View (Root views) ())
  -> Eff es Response
runLoad page = do
  ev <- (.event) <$> send GetRequest
  case ev of
    Just rawEvent -> do
      res <- runHandlers @views rawEvent
      case res of
        -- if we found an event, it should have been handled by one of the views
        Nothing -> respondError $ ErrNotHandled rawEvent
        Just r -> pure r
    Nothing -> do
      loadPageResponse page


loadPageResponse :: Eff es (View (Root total) ()) -> Eff es Response
loadPageResponse run = do
  vw <- run
  let vid = TargetViewId (encodedToText $ toViewId Root)
  let res = Response vid $ addContext Root vw
  pure res


-- despite not needing any effects, this must be in Eff es to get `es` on the RHS
decodeEvent :: forall id es. (HyperView id es) => Event TargetViewId Encoded -> Eff es (Maybe (Event id (Action id)))
decodeEvent (Event (TargetViewId ti) eact) =
  pure $ do
    vid <- decodeViewId ti
    act <- either (const Nothing) Just $ parseAction eact
    pure $ Event vid act
