module Example.Docs.Sessions where

import Web.Hyperbole
import Web.Hyperbole.Data.QueryData (DefaultParam (..))

data AppColor
  = White
  | Red
  | Green
  deriving (Show, Read, Eq, ToParam, FromParam)
instance DefaultParam AppColor where
  defaultParam = White

instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Red = "#F00"
  colorValue Green = "#0F0"

data Preferences = Preferences
  { color :: AppColor
  }
  deriving (Generic, ToQuery, FromQuery)

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
    setSession prefs
    pure $ el (bg prefs.color) "Custom Background"
