{-# LANGUAGE LambdaCase #-}

module Example.Page.DataLists.DataTable where

import Data.List (sortOn)
import Data.Text (pack)
import Docs.Page
import Effectful
import Example.AppRoute as Route
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages)
import Example.View.Layout
import Example.View.SortableTable (dataTable, sortBtn, sortColumn)
import Web.Atomic.CSS
import Web.Hyperbole
import Prelude hiding (even, odd)

-- DataTable -> do
--   el "Complex reusable View Functions allow us to "

page :: (Hyperbole :> es) => Page es '[Languages]
page = do
  pure $ layout (Data SortableTable) $ do
    example (Data SortableTable) $ do
      el "We can write view Functions that work in any view, like a dataTable"
      col ~ embed $ hyper Languages $ languagesView Nothing allLanguages

data Languages = Languages
  deriving (Generic, ViewId)

data SortField
  = SortName
  | SortDescription
  | SortFamily
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToParam, FromParam)

instance HyperView Languages es where
  data Action Languages
    = SortOn SortField
    deriving (Generic, ViewAction)

  update (SortOn fld) = do
    let sorted = sortOnField fld allLanguages
    pure $ languagesView (Just fld) sorted

sortOnField :: SortField -> [ProgrammingLanguage] -> [ProgrammingLanguage]
sortOnField = \case
  SortName -> sortOn (.name)
  SortDescription -> sortOn (.description)
  SortFamily -> sortOn (.family)

languagesView :: Maybe SortField -> [ProgrammingLanguage] -> View Languages ()
languagesView fld langs =
  table langs ~ dataTable $ do
    sortColumn (sortBtn "Language" (SortOn SortName) (fld == Just SortName)) (.name)
    sortColumn (sortBtn "Family" (SortOn SortFamily) (fld == Just SortFamily)) $ \d -> pack $ show d.family
    sortColumn (sortBtn "Description" (SortOn SortDescription) (fld == Just SortDescription)) (.description)
