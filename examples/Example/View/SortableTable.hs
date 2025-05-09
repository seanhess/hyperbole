module Example.View.SortableTable where

import Data.Text (Text)
import Effectful.Writer.Static.Local
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

alternatingRows :: (Styleable a) => CSS a -> CSS a
alternatingRows = odd (bg White) . even (bg Light)

bord :: (Styleable a) => CSS a -> CSS a
bord = border 1 . borderColor Light

hd :: View id () -> TableHead id ()
hd = th ~ pad 4 . bord . bg Light

cell :: (Styleable a) => CSS a -> CSS a
cell = pad 4 . bord

dataTable :: (Styleable a) => CSS a -> CSS a
dataTable = alternatingRows

sortColumn :: (ViewAction (Action id)) => Text -> Action id -> Bool -> (dt -> Text) -> Eff '[Writer [TableColumn id dt]] ()
sortColumn lbl click isSelected cellText =
  tcol (hd sortBtn) $ \item -> td ~ cell $ text $ cellText item
 where
  sortBtn = button click ~ Style.link . flexRow . gap 0 $ do
    el ~ selectedColumn $ text lbl
    el ~ width 20 $ Icon.chevronDown

  selectedColumn =
    if isSelected
      then underline
      else id
