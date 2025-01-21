{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Autocomplete where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages, isMatchLanguage)
import Example.Page.Filter as Filter (chosenView, resultsTable)
import Example.View.Layout (exampleLayout)
import Safe (atMay)
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es) => Eff es (Page '[LiveSearch])
page = do
  pure $ exampleLayout Route.Autocomplete $ col (pad 20 . grow) $ do
    hyper LiveSearch $ liveSearchView allLanguages 0 mempty

data LiveSearch = LiveSearch
  deriving (Show, Read, ViewId)

type Term = Text

instance (IOE :> es) => HyperView LiveSearch es where
  data Action LiveSearch
    = SearchTerm Int Term
    | Select (Maybe ProgrammingLanguage)
    deriving (Show, Read, ViewAction)

  update (SearchTerm current term) = do
    pure $ liveSearchView allLanguages current term
  update (Select Nothing) = do
    pure $ liveSearchView allLanguages 0 mempty
  update (Select (Just lang)) = do
    pure $ selectedView lang

selectedView :: ProgrammingLanguage -> View LiveSearch ()
selectedView selected = do
  col (gap 10) $ do
    Filter.chosenView selected

liveSearchView :: [ProgrammingLanguage] -> Int -> Term -> View LiveSearch ()
liveSearchView langs current term = do
  col (gap 10) $ do
    stack (bg Danger) $ do
      layer flexCol $ do
        search (SearchTerm current) 200 (searchKeys . placeholder "search programming languages" . border 1 . pad 10 . grow)
      -- Filter.clearButton (SearchTerm current) term
      layer (popup (TRBL 50 0 0 0) . shownIfMatches) $ do
        searchPopup matchedLanguages currentSearchLang
    Filter.resultsTable (Select . Just) langs
 where
  matchedLanguages = filter (isMatchLanguage term) langs

  currentSearchLang = matchedLanguages `atMay` current

  -- Only show the search popup if there is a search term and matchedLanguages
  shownIfMatches =
    if T.null term || null matchedLanguages then hide else flexCol

  searchKeys =
    onKeyDown Enter (Select currentSearchLang)
      . onKeyDown ArrowDown (SearchTerm (current + 1) term)
      . onKeyDown ArrowUp (SearchTerm (current - 1) term)

searchPopup :: [ProgrammingLanguage] -> Maybe ProgrammingLanguage -> View LiveSearch ()
searchPopup shownLangs highlighted = do
  col (border 1 . bg White) $ do
    forM_ shownLangs $ \lang -> do
      button (Select (Just lang)) (hover (bg Light) . selected lang . pad 5) $ do
        text lang.name
 where
  selected l = if Just l == highlighted then bg Light else id
