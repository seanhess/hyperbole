module Simple where

import Data.Text (Text)
import Web.Hyperbole


main :: IO ()
main = do
  run 3000 $ do
    liveApp (basicDocument "Example") $ page $ do
      hyper message
      load $ do
        pure $ viewId Msg $ viewMsg "HELLO WORLD"


data Msg = Msg
  deriving (Generic, Param)
instance HyperView Msg where
  type Action Msg = MsgAction


data MsgAction = SetMsg Text
  deriving (Generic, Param)


message :: Msg -> MsgAction -> Eff es (View Msg ())
message _ (SetMsg m) = do
  pure $ viewMsg m


viewMsg :: Text -> View Msg ()
viewMsg m = col id $ do
  el_ "Message:"
  el_ $ text m
  button (SetMsg "Goodbye") id "Goodbye"
