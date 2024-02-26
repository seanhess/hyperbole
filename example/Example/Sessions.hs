module Example.Sessions where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Example.Colors
import Example.Effects.Debug
import Web.Hyperbole


-- this is already running in a different context
page :: (Hyperbole :> es, Debug :> es) => Page es Response
page = do
  hyper content

  load $ do
    -- setSession "color" Warning
    -- setSession "msg" ("________" :: Text)
    (clr :: Maybe AppColor) <- session "color"
    (msg :: Maybe Text) <- session "msg"
    pure $ row (pad 20) $ do
      viewId Contents $ viewContent clr msg


data Contents = Contents
  deriving (Show, Read, Param)
instance HyperView Contents where
  type Action Contents = ContentsAction


data ContentsAction
  = SaveColor AppColor
  | SaveMessage Text
  deriving (Show, Read, Param)


content :: (Hyperbole :> es, Debug :> es) => Contents -> ContentsAction -> Eff es (View Contents ())
content _ (SaveColor clr) = do
  setSession "color" clr
  msg <- session "msg"
  pure $ viewContent (Just clr) msg
content _ (SaveMessage msg) = do
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
      button (SaveColor Success) (btn Success) "Successs"
      button (SaveColor Warning) (btn Warning) "Warning"
      button (SaveColor Error) (btn Error) "Error"


viewMessage :: Maybe Text -> View Contents ()
viewMessage mm = do
  let msg = fromMaybe "" mm
  col (gap 10 . pad 20 . border 1) $ do
    el (fontSize 24 . bold) "Session Message:"
    el_ $ text msg
    row (gap 10) $ do
      button (SaveMessage "Hello") (btn White) "Msg: Hello"
      button (SaveMessage "Goodbye") (btn White) "Msg: Goodbye"
      button (SaveMessage "________") (btn White) "Clear"


btn :: AppColor -> Mod
btn clr = bg clr . color Dark . pad 10 . border 1
