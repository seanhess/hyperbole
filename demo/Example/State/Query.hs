{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.State.Query where

import Data.Text (Text)
import Effectful
import Example.Colors
import Example.Style qualified as Style
import Example.Style.Cyber (btn', btnLight)
import Web.Atomic.CSS
import Web.Hyperbole

data Preferences = Preferences
  { message :: Text
  , color :: AppColor
  }
  deriving (Generic, Show, ToQuery, FromQuery)
instance Default Preferences where
  def = Preferences mempty def

page :: (Hyperbole :> es) => Page es '[QueryPrefs]
page = do
  prefs <- query @Preferences
  pure $ do
    hyper QueryPrefs $ viewPreferences prefs

data QueryPrefs = QueryPrefs
  deriving (Generic, ViewId)

instance HyperView QueryPrefs es where
  data Action QueryPrefs
    = SaveColor AppColor
    | SaveMessage Text
    | Clear
    deriving (Generic, ViewAction)
  update (SaveColor clr) = do
    prefs <- saveColor clr
    pure $ viewPreferences prefs
  update (SaveMessage msg) = do
    prefs <- modifyQuery $ \p -> p{message = msg}
    pure $ viewPreferences prefs
  update Clear = do
    setQuery @Preferences def
    pure $ viewPreferences def

saveColor :: (Hyperbole :> es) => AppColor -> Eff es Preferences
saveColor clr =
  modifyQuery $ \p -> p{color = clr}

viewPreferences :: Preferences -> View QueryPrefs ()
viewPreferences prefs = do
  col ~ gap 20 $ do
    viewColorPicker prefs.color
    viewMessage prefs.message
    button Clear ~ Style.btnLight $ "Clear"

viewColorPicker :: AppColor -> View QueryPrefs ()
viewColorPicker clr = do
  col ~ gap 10 . pad 20 . bg clr . border 1 $ do
    el ~ fontSize 18 . bold $ "Query Background"
    row ~ gap 10 $ do
      button (SaveColor Success) ~ (btn' Success . brd) $ "Successs"
      button (SaveColor Warning) ~ (btn' Warning . brd) $ "Warning"
      button (SaveColor Danger) ~ (btn' Danger . brd) $ "Danger"
 where
  brd = border $ TRBL 1 0 0 1

viewMessage :: Text -> View QueryPrefs ()
viewMessage msg = do
  col ~ gap 10 . pad 20 . border 1 $ do
    el ~ fontSize 18 . bold $ "Query Message"
    el $ text msg
    row ~ gap 10 $ do
      button (SaveMessage "Hello") ~ btnLight $ "Msg: Hello"
      button (SaveMessage "Goodbye") ~ btnLight $ "Msg: Goodbye"
