module Web.Hyperbole.Event (Event (..)) where


-- | An action, with its corresponding id
data Event id act = Event
  { viewId :: id
  , action :: act
  }


instance (Show act, Show id) => Show (Event id act) where
  show e = "Event " <> show e.viewId <> " " <> show e.action
