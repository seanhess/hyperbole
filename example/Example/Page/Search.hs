{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Search where

import Control.Monad (forM_)
import Data.List ((!?))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Example.AppRoute qualified as Route
import Example.Colors
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages, isMatchLanguage)
import Example.Page.Filter as Filter (chosenView, resultsTable)
import Example.View.Layout (exampleLayout)
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es) => Eff es (Page '[LiveSearch])
page = do
  pure $ exampleLayout Route.LiveSearch $ col (pad 20 . grow) $ do
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
    stack id $ do
      layer $ search (SearchTerm current) 200 (searchKeys . placeholder "search programming languages" . border 1 . pad 10)
      -- Filter.clearButton (SearchTerm current) term
      searchPopup matchedLanguages currentSearchLang shownIfTerm
    Filter.resultsTable (Select . Just) langs
 where
  matchedLanguages = filter (isMatchLanguage term) langs

  currentSearchLang = matchedLanguages !? current

  -- Only show the search popup if there is a search term
  shownIfTerm = if T.null term then hide else flexCol

  searchKeys =
    onKeyDown Enter (Select currentSearchLang)
      . onKeyDown ArrowDown (SearchTerm (current + 1) term)
      . onKeyDown ArrowUp (SearchTerm (current - 1) term)

searchPopup :: [ProgrammingLanguage] -> Maybe ProgrammingLanguage -> Mod LiveSearch -> Layer LiveSearch ()
searchPopup shownLangs highlighted f = do
  popout (offset (TRBL 50 0 0 0) . border 1 . bg White . f) $ do
    forM_ shownLangs $ \lang -> do
      button (Select (Just lang)) (hover (bg Light) . selected lang . pad 5) $ do
        text lang.name
 where
  selected l = if Just l == highlighted then bg Light else id
