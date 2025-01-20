module Example.View.SortableTable where

import Data.Text (Text)
import Effectful.Writer.Static.Local
import Example.Colors
import Example.Style qualified as Style
import Web.Hyperbole
import Prelude hiding (even, odd)

dataRow :: Mod c
dataRow = gap 10 . pad (All $ PxRem dataRowPadding)

dataRowPadding :: PxRem
dataRowPadding = 5

alternatingRows :: Mod c
alternatingRows = odd (bg White) . even (bg Light)

bord :: Mod c
bord = border 1 . borderColor Light

hd :: View id () -> View (TableHead id) ()
hd = th (pad 4 . bord . bold . bg Light)

cell :: Mod c
cell = pad 4 . bord

dataTable :: Mod c
dataTable = alternatingRows

sortColumn :: (ViewAction (Action id)) => Text -> Action id -> Mod () -> (dt -> Text) -> Eff '[Writer [TableColumn id dt]] ()
sortColumn lbl click f cellText =
  tcol (hd sortBtn) $ \item -> td (cell . f) $ text $ cellText item
 where
  sortBtn = button click Style.link (text lbl)
