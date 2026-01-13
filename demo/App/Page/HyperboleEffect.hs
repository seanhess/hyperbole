module App.Page.HyperboleEffect where

import App.Route as Route hiding (Response, UserId)
import App.Docs
import Effectful
import Example.Errors (Errors (..), Users (..), viewCustom, viewExceptions, viewKnownUsers, viewSearchUsers)
import Example.Errors qualified as Errors
import Example.Requests (CheckRequest (..), ControlClient (..), ControlResponse (..))
import Example.Requests qualified as Requests
import Example.View.Layout (layoutSubnav)
import Web.Hyperbole hiding (Response)

data Sections
  = Requests
  | Response
  | ExceptionHandling
  | EdgeCases
  | HandleInViews
  | CustomErrorViews
  deriving (Show, Enum, Bounded, PageAnchor)

page :: (Hyperbole :> es) => Page es '[CheckRequest, ControlResponse, ControlClient, Errors, Users]
page = do
  r <- request
  pure $ layoutSubnav @Sections Route.HyperboleEffect $ do
    section Requests $ do
      markdocs "The `Hyperbole` `Effect` allows us to skip the normal update cycle to directly access the `Request` or manipulate the `Client`"
      example Requests.source $ do
        hyper CheckRequest $ Requests.viewRequest r

      example Requests.source $ do
        hyper ControlClient Requests.viewClient

    section Response $ do
      el "It also allows us to directly affect the response and the javascript client"
      example Requests.source $ hyper ControlResponse Requests.responseView

    section ExceptionHandling $ do
      el "Any uncaught exceptions thrown from a handler will be displayed in a bright red box inline in the corresponding HyperView"
      example Errors.source $ do
        hyper Exceptions viewExceptions

    section EdgeCases $ do
      el "You can use the same mechanism to exit execution early and display an application error to handle edge cases"
      example Errors.source $ do
        hyper KnownUsers viewKnownUsers

    section HandleInViews $ do
      el "Handle any expected errors in your view function, by making it accept a Maybe or Either"
      example Errors.source $ do
        hyper SearchUsers viewSearchUsers

    section CustomErrorViews $ do
      el "You can also exit execution early and display a custom view from application code or from caught execptions"
      example Errors.source $ do
        hyper Customs viewCustom
