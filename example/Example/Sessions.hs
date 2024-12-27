{-# LANGUAGE UndecidableInstances #-}

module Example.Sessions where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Effects.Debug
import Example.Style as Style
import Example.View.Layout (exampleLayout)
import Web.Hyperbole

page :: (Hyperbole :> es, Debug :> es) => Eff es (Page '[Contents])
page = do
  -- setSession "color" Warning
  -- setSession "msg" ("________" :: Text)
  (clr :: Maybe AppColor) <- session "color"
  (msg :: Maybe Text) <- session "msg"
  pure $ exampleLayout Route.Sessions $ col (pad 20 . gap 10) $ do
    hyper Contents $ viewContent clr msg

data Contents = Contents
  deriving (Show, Read, ViewId)

instance (Debug :> es) => HyperView Contents es where
  data Action Contents
    = SaveColor AppColor
    | SaveMessage Text
    deriving (Show, Read, ViewAction)
  update (SaveColor clr) = do
    setSession "color" clr
    msg <- session "msg"
    pure $ viewContent (Just clr) msg
  update (SaveMessage msg) = do
    setSession "msg" msg
    clr <- session "color"
    pure $ viewContent clr (Just msg)

viewContent :: Maybe AppColor -> Maybe Text -> View Contents ()
viewContent mclr mmsg =
  col (gap 20) $ do
    viewColorPicker mclr
    viewMessage mmsg

viewColorPicker :: Maybe AppColor -> View Contents ()
viewColorPicker mc = do
  let clr = fromMaybe White mc
  col (gap 10 . pad 20 . bg clr . border 1) $ do
    el (fontSize 24 . bold) "Session Background"
    row (gap 10) $ do
      button (SaveColor Success) (Style.btn' Success . border 1) "Successs"
      button (SaveColor Warning) (Style.btn' Warning . border 1) "Warning"
      button (SaveColor Danger) (Style.btn' Danger . border 1) "Danger"

viewMessage :: Maybe Text -> View Contents ()
viewMessage mm = do
  let msg = fromMaybe "" mm
  col (gap 10 . pad 20 . border 1) $ do
    el (fontSize 24 . bold) "Session Message:"
    el_ $ text msg
    row (gap 10) $ do
      button (SaveMessage "Hello") Style.btnLight "Msg: Hello"
      button (SaveMessage "Goodbye") Style.btnLight "Msg: Goodbye"
      button (SaveMessage "________") Style.btnLight "Clear"
