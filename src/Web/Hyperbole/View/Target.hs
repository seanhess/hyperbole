{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Hyperbole.View.Target where

import Web.Hyperbole.Component (Component (..))
import Web.Hyperbole.HyperView
import Web.View (Mod, View, addContext, att, el, flexCol)


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

-- TODO: if I'm going to limit it, it's going to happen here
-- AND all their children have to be there
-- , All (Elem (Require ctx)) (Require id)

hyperUnsafe :: (ViewId c, Component c es) => c -> View c () -> View ctx ()
hyperUnsafe vid vw = do
  el (att "id" (toViewId vid) . flexCol) $
    addContext vid vw


-- | Internal
dataTarget :: (ViewId a) => a -> Mod
dataTarget = att "data-target" . toViewId


{- | Trigger actions for another view. They will update the view specified

> otherView :: View OtherView ()
> otherView = do
>   el_ "This is not a message view"
>   button OtherAction id "Do Something"
>
>   target (Message 2) $ do
>     el_ "Now we can trigger a MessageAction which will update our Message HyperView, not this one"
>     button ClearMessage id "Clear Message #2"
-}
target :: (Component id es) => id -> View id () -> View a ()
target = addContext
