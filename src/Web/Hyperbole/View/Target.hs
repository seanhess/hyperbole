module Web.Hyperbole.View.Target where

import Web.Hyperbole.Handler.TypeList ()


{- | Embed HyperViews into the page, or nest them into other views

@
myPage :: ('Hyperbole' :> es) => 'Page' es 'Response'
myPage = do
  'handle' messages
  'load' $ do
    pure $ do
      'el_' "My Page"
      'hyper' (Message 1) $ messageView "Hello World"
      'hyper' (Message 2) $ do
        messageView "Another Message"
        'hyper' OtherView otherView
@

Views can only trigger actions that match their HyperView

@
messageView :: Text -> View Message ()
messageView m = do
  el_ (text m)
  button (Louder m) \"Louder\"

otherView :: View OtherView ()
otherView = do
  -- Type Error!
  button (Louder \"Hi\") id \"Louder\"
@
-}
