module Example.Sessions where

import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Example.Colors
import Example.Effects.Debug
import Text.Read (readMaybe)
import Web.Hyperbole


-- this is already running in a different context
page :: (Hyperbole :> es, IOE :> es, Debug :> es) => Page es Response
page = do
  hyper content

  load $ do
    setSession "color" Error
    (clr :: Maybe AppColor) <- session "color"
    dump "COLOR" clr
    pure $ row (pad 20) $ do
      viewId Contents $ viewContent clr


data Contents = Contents
  deriving (Show, Read, Param)
instance HyperView Contents where
  type Action Contents = ContentsAction


data ContentsAction
  = SaveColor AppColor
  deriving (Show, Read, Param)


content :: (Hyperbole :> es) => Contents -> ContentsAction -> Eff es (View Contents ())
content _ (SaveColor clr) = do
  -- TODO: need to set session in socket connection
  setSession "color" (show clr)
  pure $ viewContent (Just clr)


viewContent :: Maybe AppColor -> View Contents ()
viewContent mc = do
  let clr = fromMaybe White mc
  col (gap 10 . pad 20 . bg clr) $ do
    el (fontSize 24 . bold) "Session Background"
    row (gap 10) $ do
      button (SaveColor Success) (btn Success) "Successs"
      button (SaveColor Warning) (btn Warning) "Warning"
      button (SaveColor Error) (btn Error) "Error"


-- button Expand btn "Expand"

-- viewBig :: View Contents ()
-- viewBig = col (gap 10 . border 1 . pad 20 . transition 300 (Height 400)) $ do
--   el_ "One"
--   el_ "TWO"
--   button Collapse (bg Secondary . hover (bg SecondaryLight) . color White . pad 10) "Collapse"

btn :: AppColor -> Mod
btn clr = bg clr . color Dark . pad 10 . border 1
