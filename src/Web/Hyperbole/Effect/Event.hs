module Web.Hyperbole.Effect.Event where


-- -- despite not needing any effects, this must be in Eff es to get `es` on the RHS
-- decodeEvent :: (HyperView id es) => Event TargetViewId Text -> Eff es (Maybe (Event id (Action id)))
-- decodeEvent (Event (TargetViewId ti) ta) =
--   pure $ do
--     vid <- decodeViewId ti
--     act <- decodeAction ta
--     pure $ Event vid act
