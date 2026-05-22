{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.DataLists.Autocomplete where

import App.Docs
import App.Route as Route
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Example.Colors
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages, isMatchLanguage)
import Example.DataLists.Filter as Filter (chosenView, clearButton, resultsTable)
import Example.View.Layout
import Safe (atMay)
import Web.Atomic.CSS
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es) => Page es '[LiveSearch]
page = do
  pure $ layout (Data Autocomplete) $ do
    el "Create a serverside autocomplete with a combination of onInput and onKeyDown"
    example $(moduleSource) $ do
      hyper LiveSearch $ liveSearchView allLanguages 0 ""

data LiveSearch = LiveSearch
  deriving (Generic, ViewId)

instance (IOE :> es) => HyperView LiveSearch es where
  data Action LiveSearch
    = SearchTerm Int (Maybe Text)
    | Select (Maybe ProgrammingLanguage)
    deriving (Generic, ViewAction)

  -- favor the latest thing typed
  type Concurrency LiveSearch = Replace

  update (SearchTerm current term) = do
    val <- inputValue
    pure $ liveSearchView allLanguages current (fromMaybe "" $ term <|> val)
  update (Select Nothing) = do
    pure $ liveSearchView allLanguages 0 ""
  update (Select (Just lang)) = do
    pure $ selectedView lang

selectedView :: ProgrammingLanguage -> View LiveSearch ()
selectedView selected = do
  Filter.chosenView selected (Select Nothing)

liveSearchView :: [ProgrammingLanguage] -> Int -> Text -> View LiveSearch ()
liveSearchView langs current term = do
  col ~ gap 10 $ do
    el ~ stack $ do
      search (SearchTerm current Nothing) 250 @ searchKeys . placeholder "search programming languages" . value term . autofocus ~ border 1 . pad 10 . grow
      Filter.clearButton (SearchTerm current Nothing) term
      col ~ popup (TRBL 50 0 0 0) . shownIfMatches $ do
        searchPopup matchedLanguages currentSearchLang
    Filter.resultsTable (Select . Just) langs
 where
  matchedLanguages = filter (isMatchLanguage term) langs

  currentSearchLang = matchedLanguages `atMay` current

  -- Only show the search popup if there is a search term and matchedLanguages
  shownIfMatches =
    if T.null term || null matchedLanguages then display None else flexCol

  -- TEST: this will clear the user input??? Before, we passed it along, so we could keep it the same as you arrowed around
  -- that's not good! How can we fix it?
  searchKeys =
    onKeyDown Enter (Select currentSearchLang)
      . onKeyDown ArrowDown (SearchTerm (current + 1) (Just term))
      . onKeyDown ArrowUp (SearchTerm (current - 1) (Just term))

searchPopup :: [ProgrammingLanguage] -> Maybe ProgrammingLanguage -> View LiveSearch ()
searchPopup shownLangs highlighted = do
  col ~ border 1 . bg White $ do
    forM_ shownLangs $ \lang -> do
      button (Select (Just lang)) ~ hover (bg Light) . selected lang . pad 5 $ do
        text lang.name
 where
  selected l = if Just l == highlighted then bg Light else id
