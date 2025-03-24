{-# LANGUAGE LambdaCase #-}

module Example.Page.DataTable where

import Data.List (sortOn)
import Data.Text (pack)
import Effectful
import Example.AppRoute qualified as Route
import Example.Data.ProgrammingLanguage (ProgrammingLanguage (..), allLanguages)
import Example.View.Layout (exampleLayout)
import Example.View.SortableTable (dataTable, sortColumn)
import Web.Hyperbole
import Prelude hiding (even, odd)

page :: (Hyperbole :> es) => Eff es (Page '[Languages])
page = do
  pure $ exampleLayout Route.DataTable $ col (pad 20 . grow) $ do
    hyper Languages $ languagesView Nothing allLanguages

data Languages = Languages
  deriving (Generic, ViewId)

data SortField
  = SortName
  | SortDescription
  | SortFamily
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

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
  table dataTable langs $ do
    sortColumn "Language" (SortOn SortName) (fld == Just SortName) id (.name)
    sortColumn "Family" (SortOn SortFamily) (fld == Just SortFamily) id $ \d -> pack $ show d.family
    sortColumn "Description" (SortOn SortDescription) (fld == Just SortDescription) id (.description)
