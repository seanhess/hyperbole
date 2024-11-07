{-# LANGUAGE UndecidableInstances #-}

module Example.Sessions where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Example.Colors
import Example.Effects.Debug
import Example.Style as Style
import Web.Hyperbole


-- this is already running in a different context
page :: (Hyperbole :> es, Debug :> es) => Eff es (Page '[Contents])
page = do
  -- setSession "color" Warning
  -- setSession "msg" ("________" :: Text)
  (clr :: Maybe AppColor) <- session "color"
  (msg :: Maybe Text) <- session "msg"
  pure $ col (pad 20 . gap 10) $ do
    el_ "Reload your browser after changing the settings below to see the session information preserved"
    row id $ do
      hyper Contents $ viewContent clr msg


data Contents = Contents
  deriving (Show, Read, ViewId)


data ContentsAction
  = SaveColor AppColor
  | SaveMessage Text
  deriving (Show, Read, ViewAction)


instance HyperView Contents where
  type Action Contents = ContentsAction
instance (Debug :> es) => Handle Contents es where
  handle _ (SaveColor clr) = do
    setSession "color" clr
    msg <- session "msg"
    pure $ viewContent (Just clr) msg
  handle _ (SaveMessage msg) = do
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
  col (gap 10 . pad 20 . bg clr) $ do
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
