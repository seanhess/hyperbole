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
  -- this part never changes
  el bold "My Page"
  -- register a view with Id = Message, which updates itself with vdom
  viewId Message $ messageView "HELLO WORLD"


-- Unique View Id
data Message = Message
  deriving (Generic, Param)


-- Actions for Message Views
data MessageAction = SetMessage Text
  deriving (Generic, Param)


instance HyperView Message where
  type Action Message = MessageAction


-- Handle Message actions
message :: Message -> MessageAction -> Eff es (View Message ())
message _ (SetMessage m) = do
  -- After side effects, re-render the view with new data
  pure $ messageView m


-- Render a message view
messageView :: Text -> View Message ()
messageView m = col id $ do
  el_ "Message:"
  el_ $ text m
  button (SetMessage "Goodbye") id "Goodbye"
