module Example.View.SortableTable where

import Data.Text (Text)
import Example.Colors
import Example.View.Icon qualified as Icon
import Web.Atomic.CSS
import Web.Hyperbole
import Prelude hiding (even, odd)

data SortDirection
  = Ascending
  | Descending
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

dataRow :: (Styleable a) => CSS a -> CSS a
dataRow = gap 10 . pad (All $ PxRem dataRowPadding)

dataRowPadding :: PxRem
dataRowPadding = 5

bord :: (Styleable a) => CSS a -> CSS a
bord = border 1 . borderColor Light

hd :: View id () -> TableHead id ()
hd = th ~ pad 4 . bord . bg Light

cell :: (Styleable a) => CSS a -> CSS a
cell = pad 4 . bord

dataTable :: (Styleable a) => CSS a -> CSS a
dataTable =
  css
    "data-table"
    ".data-table tr:nth-child(even)"
    (declarations (bg Light))

sortBtn :: (ViewAction (Action id)) => Text -> Action id -> Maybe SortDirection -> View id ()
sortBtn lbl click mDir =
  button click ~ sortStyle . flexRow . utility "items-center" ["align-items" :. "center"] . gap 4 $ do
    el $ text lbl
    el ~ width 16 $ sortIcon
 where
  sortStyle = case mDir of
    Nothing -> color Secondary
    Just _ -> color Primary . underline
  sortIcon = case mDir of
    Nothing -> el ~ color SecondaryLight $ Icon.chevronUpDown
    Just Ascending -> el Icon.chevronDown
    Just Descending -> el Icon.chevronUp

sortColumn :: (ViewAction (Action id)) => View id () -> (dt -> Text) -> TableColumns id dt ()
sortColumn header cellText = do
  tcol (hd header) $ \item ->
    td ~ cell $ text $ cellText item
