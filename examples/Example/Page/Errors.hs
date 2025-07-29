{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Errors where

import Effectful
import Effectful.Exception
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Style as Style
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.Effect.Response (respondError)
import Web.Hyperbole.Effect.Server.Response (ResponseError (..))

page :: (Hyperbole :> es) => Eff es (Page '[Contents])
page = do
  pure $ exampleLayout Route.Errors $ do
    example "Errors" "Example/Page/Errors.hs" $ do
      el "Blah blah blah"
      col ~ embed $ do
        hyper Contents viewContent

data Contents = Contents
  deriving (Generic, ViewId)

instance (IOE :> es) => HyperView Contents es where
  data Action Contents
    = CauseNotFound
    | CauseServerside
    | CauseUserFacing
    | CauseCustom
    deriving (Generic, ViewAction)

  update CauseNotFound = do
    notFound
  update CauseServerside = do
    -- You can catch any error in handler effects
    -- and use `respondError` to display a custom error rather than the default
    throwIO $ SomeServerError "Oh no!"
  update CauseUserFacing = do
    respondError "This is a user-facing custom error"
  update CauseCustom = do
    respondError $ ErrCustom "something" $ do
      el ~ border 1 . borderColor Danger . rounded 3 $ "Style me however you want!"

viewContent :: View Contents ()
viewContent = do
  col ~ gap 10 $ do
    button CauseNotFound ~ Style.btn $ "Not Found Error"
    button CauseServerside ~ Style.btn $ "Serverside Error"
    button CauseUserFacing ~ Style.btn $ "Custom Error Message"
    button CauseCustom ~ Style.btn $ "Custom Error Body"

data SomeServerError
  = SomeServerError String
  deriving (Show, Eq, Exception)
