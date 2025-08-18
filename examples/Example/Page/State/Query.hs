{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.State.Query where

import Data.Text (Text)
import Effectful
import Example.AppRoute as Route
import Example.Colors
import Example.Style as Style
import Example.View.Layout (embed, example, exampleLayout)
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
  pure $ exampleLayout (State Query) $ do
    example "Query" "Example/Page/Query.hs" $ do
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
      button (SaveColor Success) ~ (Style.btn' Success . border 1) $ "Successs"
      button (SaveColor Warning) ~ (Style.btn' Warning . border 1) $ "Warning"
      button (SaveColor Danger) ~ (Style.btn' Danger . border 1) $ "Danger"

viewMessage :: Text -> View Contents ()
viewMessage msg = do
  col ~ gap 10 . pad 20 . border 1 $ do
    el ~ fontSize 18 . bold $ "Query Message"
    el $ text msg
    row ~ gap 10 $ do
      button (SaveMessage "Hello") ~ Style.btnLight $ "Msg: Hello"
      button (SaveMessage "Goodbye") ~ Style.btnLight $ "Msg: Goodbye"
