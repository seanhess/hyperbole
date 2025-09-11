module Example.Docs.Sessions where

import Web.Atomic.CSS
import Web.Hyperbole

data AppColor
  = White
  | Red
  | Green
  deriving (Show, Generic, ToJSON, FromJSON)

instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Red = "#F00"
  colorValue Green = "#0F0"

data Preferences = Preferences
  { color :: AppColor
  }
  deriving (Generic, ToJSON, FromJSON, Session)
instance Default Preferences where
  def = Preferences White

page :: (Hyperbole :> es) => Page es '[Content]
page = do
  prefs <- session @Preferences
  pure $ el ~ bg prefs.color $ "Custom Background"

data Content = Content
  deriving (Generic, ViewId)

instance HyperView Content es where
  data Action Content
    = SetColor AppColor
    deriving (Generic, ViewAction)

  update (SetColor clr) = do
    let prefs = Preferences clr
    saveSession prefs
    pure $ el ~ bg prefs.color $ "Custom Background"
