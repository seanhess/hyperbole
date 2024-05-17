module Simple where

import Data.Text (Text)
import Web.Hyperbole


main :: IO ()
main = do
  -- Warp.run on port 3000
  run 3000 $ do
    -- create a WAI.Application
    liveApp (basicDocument "Example") $ do
      -- A single page
      page $ do
        -- handle message actions
        hyper message
        -- handle initial page load
        load $ do
          -- after side effects, render entire page once
          pure viewPage


-- render entire page
viewPage :: View c ()
viewPage = do
  el bold "My Page"
  -- register a view with Id = Msg, which updates itself
  viewId Msg $ viewMsg "HELLO WORLD"


-- Unique View Id
data Msg = Msg
  deriving (Generic, Param)


-- Actions for that View
data MsgAction = SetMsg Text
  deriving (Generic, Param)


instance HyperView Msg where
  type Action Msg = MsgAction


-- Handle message actions
message :: Msg -> MsgAction -> Eff es (View Msg ())
message _ (SetMsg m) = do
  -- After side effects, re-render the view with new data
  pure $ viewMsg m


-- Render a message view
viewMsg :: Text -> View Msg ()
viewMsg m = col id $ do
  el_ "Message:"
  el_ $ text m
  button (SetMsg "Goodbye") id "Goodbye"
