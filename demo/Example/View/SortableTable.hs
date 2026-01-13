module Example.View.SortableTable where

import Data.Text (Text)
import Example.Colors
import Example.Style qualified as Style
import Example.View.Icon qualified as Icon
import Web.Atomic.CSS
import Web.Hyperbole
import Prelude hiding (even, odd)

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

sortBtn :: (ViewAction (Action id)) => Text -> Action id -> Bool -> View id ()
sortBtn lbl click isSelected = do
  button click ~ Style.link . flexRow . gap 0 $ do
    el ~ selectedColumn $ (text lbl)
    el ~ width 20 $ Icon.chevronDown
 where
  selectedColumn =
    if isSelected
      then underline
      else id

sortColumn :: (ViewAction (Action id)) => View id () -> (dt -> Text) -> TableColumns id dt ()
sortColumn header cellText = do
  tcol (hd header) $ \item ->
    td ~ cell $ text $ cellText item
