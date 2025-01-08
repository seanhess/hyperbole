module Example.Docs.Sessions where

import Web.Hyperbole

data AppColor
  = White
  | Red
  | Green
  deriving (Show, Read, Eq, ToParam, FromParam)

instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Red = "#F00"
  colorValue Green = "#0F0"

data Preferences = Preferences
  { color :: AppColor
  }
  deriving (Generic, Show, Read, ToParam, FromParam, Session)
instance DefaultParam Preferences where
  defaultParam = Preferences White

page :: (Hyperbole :> es) => Eff es (Page '[Content])
page = do
  prefs <- session @Preferences
  pure $ el (bg prefs.color) "Custom Background"

data Content = Content
  deriving (Show, Read, ViewId)

instance HyperView Content es where
  data Action Content
    = SetColor AppColor
    deriving (Show, Read, ViewAction)

  update (SetColor clr) = do
    let prefs = Preferences clr
    saveSession prefs
    pure $ el (bg prefs.color) "Custom Background"
