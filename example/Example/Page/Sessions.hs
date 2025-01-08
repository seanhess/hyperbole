{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Page.Sessions where

import Data.Default
import Data.Text (Text)
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Effects.Debug
import Example.Style as Style
import Example.View.Layout (exampleLayout)
import Web.Hyperbole

data Preferences = Preferences
  { message :: Text
  , color :: AppColor
  }
  deriving (Generic, Show, Read, ToParam, FromParam, Session)
instance Default Preferences where
  def = Preferences "_" White


page :: (Hyperbole :> es, Debug :> es) => Eff es (Page '[Contents])
page = do
  prefs <- session @Preferences
  pure $ exampleLayout Route.Sessions $ col (pad 20 . gap 10) $ do
    hyper Contents $ viewContent prefs

data Contents = Contents
  deriving (Show, Read, ViewId)

instance (Debug :> es) => HyperView Contents es where
  data Action Contents
    = SaveColor AppColor
    | SaveMessage Text
    deriving (Show, Read, ViewAction)
  update (SaveColor clr) = do
    prefs <- modifySession $ \p -> p{color = clr}
    pure $ viewContent prefs
  update (SaveMessage msg) = do
    prefs <- modifySession $ \p -> p{message = msg}
    pure $ viewContent prefs

viewContent :: Preferences -> View Contents ()
viewContent prefs =
  col (gap 20) $ do
    viewColorPicker prefs.color
    viewMessage prefs.message

viewColorPicker :: AppColor -> View Contents ()
viewColorPicker clr = do
  col (gap 10 . pad 20 . bg clr . border 1) $ do
    el (fontSize 24 . bold) "Session Background"
    row (gap 10) $ do
      button (SaveColor Success) (Style.btn' Success . border 1) "Successs"
      button (SaveColor Warning) (Style.btn' Warning . border 1) "Warning"
      button (SaveColor Danger) (Style.btn' Danger . border 1) "Danger"

viewMessage :: Text -> View Contents ()
viewMessage msg = do
  col (gap 10 . pad 20 . border 1) $ do
    el (fontSize 24 . bold) "Session Message:"
    el_ $ text msg
    row (gap 10) $ do
      button (SaveMessage "Hello") Style.btnLight "Msg: Hello"
      button (SaveMessage "Goodbye") Style.btnLight "Msg: Goodbye"
      button (SaveMessage "________") Style.btnLight "Clear"
