{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.State.Query where

import App.Route as Route
import Data.Text (Text)
import Docs.Examples
import Docs.Page
import Effectful
import Example.Colors
import Example.Style qualified as Style
import Example.Style.Cyber (btn', btnLight)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.Query

data Preferences = Preferences
  { message :: Text
  , color :: AppColor
  }
  deriving (Generic, Show, ToQuery, FromQuery)
instance Default Preferences where
  def = Preferences mempty def

page :: (Hyperbole :> es) => Page es '[Contents]
page = do
  prefs <- query @Preferences
  pure $ layout State $ do
    example $(exampleSource) $ do
      el "We can persist state in the query string. This is useful for faceted search, or any time when a user might want to share the url include local state changes"
      col ~ embed $ hyper Contents $ viewContent prefs

data Contents = Contents
  deriving (Generic, ViewId)

instance HyperView Contents es where
  data Action Contents
    = SaveColor AppColor
    | SaveMessage Text
    | Clear
    deriving (Generic, ViewAction)
  update (SaveColor clr) = do
    prefs <- modifyQuery $ \p -> p{color = clr}
    pure $ viewContent prefs
  update (SaveMessage msg) = do
    prefs <- modifyQuery $ \p -> p{message = msg}
    pure $ viewContent prefs
  update Clear = do
    setQuery @Preferences def
    pure $ viewContent def

viewContent :: Preferences -> View Contents ()
viewContent prefs = do
  col ~ gap 20 $ do
    viewColorPicker prefs.color
    viewMessage prefs.message
    button Clear ~ Style.btnLight $ "Clear"

viewColorPicker :: AppColor -> View Contents ()
viewColorPicker clr = do
  col ~ gap 10 . pad 20 . bg clr . border 1 $ do
    el ~ fontSize 18 . bold $ "Query Background"
    row ~ gap 10 $ do
      button (SaveColor Success) ~ (btn' Success . brd) $ "Successs"
      button (SaveColor Warning) ~ (btn' Warning . brd) $ "Warning"
      button (SaveColor Danger) ~ (btn' Danger . brd) $ "Danger"
 where
  brd = border $ TRBL 1 0 0 1

viewMessage :: Text -> View Contents ()
viewMessage msg = do
  col ~ gap 10 . pad 20 . border 1 $ do
    el ~ fontSize 18 . bold $ "Query Message"
    el $ text msg
    row ~ gap 10 $ do
      button (SaveMessage "Hello") ~ btnLight $ "Msg: Hello"
      button (SaveMessage "Goodbye") ~ btnLight $ "Msg: Goodbye"
