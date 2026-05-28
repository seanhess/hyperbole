{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.DataLists.DataTable where

import App.Docs
import App.Route as Route
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text (pack)
import Effectful
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages)
import Example.View.Layout
import Example.View.SortableTable (SortDirection (..), dataTable, sortBtn, sortColumn)
import Web.Atomic.CSS
import Web.Hyperbole
import Web.Hyperbole.HyperView
import Prelude hiding (even, odd)

-- DataTable -> do
--   el "Complex reusable View Functions allow us to "

page :: (Hyperbole :> es) => Page es '[Languages]
page = do
  pure $ layout (Data SortableTable) $ do
    el "We can write view Functions that work in any view, like a dataTable"
    example $(moduleSource) $ do
      hyperState Languages Nothing $ languagesView allLanguages

data Languages = Languages
  deriving (Generic)

instance ViewId Languages where
  type ViewState Languages = Maybe (SortField, SortDirection)

data SortField
  = SortName
  | SortDescription
  | SortFamily
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

toggleSortDir :: SortDirection -> SortDirection
toggleSortDir Ascending = Descending
toggleSortDir Descending = Ascending

instance HyperView Languages es where
  data Action Languages
    = SortOn SortField
    deriving (Generic, ViewAction)

  update (SortOn fld) = do
    newDir <- gets $ \case
      Just (currFld, currDir) | currFld == fld -> toggleSortDir currDir
      _ -> Ascending
    put $ Just (fld, newDir)
    let sorted = sortOnField fld newDir allLanguages
    pure $ languagesView sorted

sortOnField :: SortField -> SortDirection -> [ProgrammingLanguage] -> [ProgrammingLanguage]
sortOnField fld dir = case fld of
  SortName -> sortBy (.name)
  SortDescription -> sortBy (.description)
  SortFamily -> sortBy (.family)
 where
  sortBy :: (Ord b) => (ProgrammingLanguage -> b) -> [ProgrammingLanguage] -> [ProgrammingLanguage]
  sortBy f = case dir of
    Ascending -> sortOn f
    Descending -> sortOn (Down . f)

languagesView :: [ProgrammingLanguage] -> View Languages ()
languagesView langs = do
  mSt <- viewState
  let directionOf fld = case mSt of
        Just (f, dir) | f == fld -> Just dir
        _ -> Nothing
      sortColumn' lbl fld =
        sortColumn (sortBtn lbl (SortOn fld) (directionOf fld))
  table langs ~ dataTable $ do
    sortColumn' "Language" SortName (.name)
    sortColumn' "Family" SortFamily $ \d -> pack $ show d.family
    sortColumn' "Description" SortDescription (.description)
