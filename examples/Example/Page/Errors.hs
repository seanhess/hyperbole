{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Errors where

import Data.Text (pack)
import Effectful
import Effectful.Exception
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Style as Style hiding (link)
import Example.View.Layout
import Web.Atomic.CSS
import Web.Hyperbole hiding (link)

page :: (Hyperbole :> es) => Eff es (Page '[Contents, Refresher])
page = do
  pure $ exampleLayout Route.Errors $ do
    example "Errors" "Example/Page/Errors.hs" $ do
      el "Blah blah blah"
      col ~ embed $ do
        hyper Contents viewContent

    example "Refresher" "Example/Page/Errors.hs" $ do
      col ~ embed $ do
        hyper Refresher (viewRefresh 0)

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
    _ <- notFound
    pure $ el "unreachable"
  update CauseServerside = do
    -- You can catch any error in handler effects
    -- and use `respondError` to display a custom error rather than the default
    _ <- throwIO $ SomeServerError "Oh no!"
    pure $ el "unreachable"
  update CauseUserFacing = do
    _ <- respondError "This is a user-facing custom error"
    pure $ el "unreachable"
  update CauseCustom = do
    _ <- respondErrorView "something" $ do
      el ~ border 1 . borderColor Danger . rounded 3 $ "Style me however you want!"
    pure $ el "unreachable"

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

data Refresher = Refresher
  deriving (Generic, ViewId)

instance HyperView Refresher es where
  data Action Refresher
    = Refresh Int
    deriving (Generic, ViewAction)

  update (Refresh n) =
    pure $ viewRefresh (n + 1)

viewRefresh :: Int -> View Refresher ()
viewRefresh n = do
  el @ onLoad (Refresh n) 500 $ text $ "Refreshing: " <> pack (show n)
