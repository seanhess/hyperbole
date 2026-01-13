{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Page.State where

import App.Route (AppRoute (State))
import App.Docs.Examples
import App.Docs.Markdown
import App.Docs.Page
import Effectful.Concurrent
import Effectful.Concurrent.STM (TVar)
import Effectful.Reader.Dynamic
import Example.Counter as Threaded
import Example.State.Effects as Effects
import Example.State.Query (QueryPrefs (..))
import Example.State.Query qualified as Query
import Example.State.Sessions qualified as Session
import Example.State.Stateless
import Example.View.Layout (layoutSubnav)
import Web.Hyperbole

data StateSection
  = Stateless
  | ActionThreading
  | ViewState
  | BrowserQuery
  | BrowserSessions
  | WithEffects
  deriving (Show, Enum, Bounded)

instance PageAnchor StateSection

page :: (Hyperbole :> es, Reader (TVar Int) :> es, Concurrent :> es) => Page es '[Threaded.Counter, Swapper, QueryPrefs, Session.Contents, Effects.Counter]
page = do
  ssn <- session @Session.Preferences
  qry <- query @Query.Preferences
  cnt <- getCount
  pure $ layoutSubnav @StateSection State $ do
    section Stateless $ do
      markdocs $(embedFile "docs/state-stateless.md")

      example $(moduleSourceNamed "Example.State.Stateless") $ do
        hyper Swapper viewSwap

    section ActionThreading $ do
      markdocs $(embedFile "docs/state-threading.md")

      example $(moduleSourceNamed "Example.Counter") $ do
        hyper Threaded.Counter $ Threaded.viewCount 0

    section ViewState $ do
      markdocs $(embedFile "docs/state-viewstate.md")

    section BrowserQuery $ do
      markdocs $(embedFile "docs/state-browser.md")

      example $(moduleSourceNamed "Example.State.Query") $ do
        hyper QueryPrefs $ Query.viewPreferences qry

    section BrowserSessions $ do
      markdocs $(embedFile "docs/state-sessions.md")

      example $(moduleSourceNamed "Example.State.Sessions") $ do
        hyper Session.Contents $ Session.viewContent ssn

    section WithEffects $ do
      markdocs $(embedFile "docs/state-effects.md")

      example $(moduleSourceNamed "Example.State.Effects") $ do
        hyper Effects.Counter $ Effects.viewCount cnt
