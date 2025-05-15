{-# LANGUAGE UndecidableInstances #-}

module Example.Page.Autocomplete where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Example.AppRoute as Route
import Example.Colors
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages, isMatchLanguage)
import Example.Page.Filter as Filter (chosenView, clearButton, resultsTable)
import Example.View.Layout
import Safe (atMay)
import Web.Atomic.CSS
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es) => Eff es (Page '[LiveSearch])
page = do
  pure $ exampleLayout (Data Autocomplete) $ do
    example "Autocomplete" "Example/Page/Autocomplete.hs" $ do
      el "Create a serverside autocomplete with a combination of onInput and onKeyDown"
      col ~ embed $ hyper LiveSearch $ liveSearchView allLanguages 0 ""

data LiveSearch = LiveSearch
  deriving (Generic, ViewId)

instance (IOE :> es) => HyperView LiveSearch es where
  data Action LiveSearch
    = SearchTerm Int Text
    | Select (Maybe ProgrammingLanguage)
    deriving (Generic, ViewAction)

  update (SearchTerm current term) = do
    pure $ liveSearchView allLanguages current term
  update (Select Nothing) = do
    pure $ liveSearchView allLanguages 0 ""
  update (Select (Just lang)) = do
    pure $ selectedView lang

selectedView :: ProgrammingLanguage -> View LiveSearch ()
selectedView selected = do
  col ~ gap 10 $ do
    Filter.chosenView selected

liveSearchView :: [ProgrammingLanguage] -> Int -> Text -> View LiveSearch ()
liveSearchView langs current term = do
  col ~ gap 10 $ do
    el ~ stack $ do
      search (SearchTerm current) 200 @ searchKeys . placeholder "search programming languages" . value term . autofocus ~ border 1 . pad 10 . grow
      Filter.clearButton (SearchTerm current) term
      col ~ popup (TRBL 50 0 0 0) . shownIfMatches $ do
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
  col ~ border 1 . bg White $ do
    forM_ shownLangs $ \lang -> do
      button (Select (Just lang)) ~ hover (bg Light) . selected lang . pad 5 $ do
        text lang.name
 where
  selected l = if Just l == highlighted then bg Light else id
