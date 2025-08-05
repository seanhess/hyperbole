{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Page.State.Sessions where

import Data.Text (Text)
import Effectful
import Example.AppRoute as Route
import Example.Colors
import Example.Style as Style
import Example.View.Layout (embed, example, exampleLayout)
import Web.Atomic.CSS
import Web.Hyperbole

data Preferences = Preferences
  { message :: Text
  , color :: AppColor
  }
  deriving (Generic, Show, ToJSON, FromJSON, FromParam, ToParam, Session)
instance Default Preferences where
  def = Preferences "_" White

page :: (Hyperbole :> es) => Eff es (Page '[Contents])
page = do
  prefs <- session @Preferences
  pure $ exampleLayout (State Sessions) $ do
    example "Sessions" "Example/Page/Sessions.hs" $ do
      el "We can also persist state in a browser cookie. This is most useful for user-specific preferences and state that should last until they clear their browser cookies"
      col ~ embed $ hyper Contents $ viewContent prefs

data Contents = Contents
  deriving (Generic, ViewId)

instance HyperView Contents es where
  data Action Contents
    = SaveColor AppColor
    | SaveMessage Text
    | ClearSession
    deriving (Generic, ViewAction)
  update (SaveColor clr) = do
    prefs <- modifySession $ \p -> p{color = clr}
    pure $ viewContent prefs
  update (SaveMessage msg) = do
    prefs <- modifySession $ \p -> p{message = msg}
    pure $ viewContent prefs
  update ClearSession = do
    deleteSession @Preferences
    pure $ viewContent def

viewContent :: Preferences -> View Contents ()
viewContent prefs = do
  col ~ gap 20 $ do
    viewColorPicker prefs.color
    viewMessage prefs.message
    button ClearSession ~ Style.btnLight $ "Clear"

viewColorPicker :: AppColor -> View Contents ()
viewColorPicker clr = do
  col ~ gap 10 . pad 20 . bg clr . border 1 $ do
    el ~ fontSize 18 . bold $ "Session Background"
    row ~ gap 10 $ do
      button (SaveColor Success) ~ (Style.btn' Success . border 1) $ "Successs"
      button (SaveColor Warning) ~ (Style.btn' Warning . border 1) $ "Warning"
      button (SaveColor Danger) ~ (Style.btn' Danger . border 1) $ "Danger"

viewMessage :: Text -> View Contents ()
viewMessage msg = do
  col ~ gap 10 . pad 20 . border 1 $ do
    el ~ fontSize 18 . bold $ "Session Message"
    el $ text msg
    row ~ (gap 10) $ do
      button (SaveMessage "Hello") ~ Style.btnLight $ "Msg: Hello"
      button (SaveMessage "Goodbye") ~ Style.btnLight $ "Msg: Goodbye"
