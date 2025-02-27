{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Web.View.Layout where

import Data.Function
import Data.Text
import Web.View.Element
import Web.View.Style
import Web.View.Types
import Web.View.View (View, tag)


{- | We can intuitively create layouts with combinations of 'row', 'col', 'stack', 'grow', and 'space'

Wrap main content in 'layout' to allow the view to consume vertical screen space

@
holygrail :: 'View' c ()
holygrail = 'layout' id $ do
  'row' section "Top Bar"
  'row' 'grow' $ do
    'col' section "Left Sidebar"
    'col' (section . 'grow') "Main Content"
    'col' section "Right Sidebar"
  'row' section "Bottom Bar"
  where section = 'border' 1
@
-}
layout :: Mod c -> View c () -> View c ()
layout f = el (root . f)


{- | As `layout` but as a 'Mod'

> holygrail = col root $ do
>   ...
-}
root :: Mod c
root = flexCol . fillViewport
 where
  fillViewport =
    addClass $
      cls "layout"
        -- [ ("white-space", "pre")
        & prop @Text "width" "100vw"
        & prop @Text "height" "100vh"
        -- not sure if this property is necessary, copied from older code
        & prop @Text "min-height" "100vh"
        & prop @Text "z-index" "0"


{- | Lay out children in a column.

> col grow $ do
>    el_ "Top"
>    space
>    el_ "Bottom"
-}
col :: Mod c -> View c () -> View c ()
col f = el (flexCol . f)


{- | Lay out children in a row

> row id $ do
>    el_ "Left"
>    space
>    el_ "Right"
-}
row :: Mod c -> View c () -> View c ()
row f = el (flexRow . f)


{- | Grow to fill the available space in the parent 'Web.View.Layout.row' or 'Web.View.Layout.col'

> row id $ do
>  el grow none
>  el_ "Right"
-}
grow :: Mod c
grow = addClass $ cls "grow" & prop @Int "flex-grow" 1


{- | Space that fills the available space in the parent 'Web.View.Layout.row' or 'Web.View.Layout.col'.


> row id $ do
>  space
>  el_ "Right"

This is equivalent to an empty element with 'grow'

> space = el grow none
-}
space :: View c ()
space = el grow none


{- | Make a fixed 'layout' by putting 'scroll' on a child-element

> document = row root $ do
>   nav (width 300) "Sidebar"
>   col (grow . scroll) "Main Content"
-}
scroll :: Mod c
scroll = addClass $ cls "scroll" & prop @Text "overflow" "auto"


-- | A Nav element
nav :: Mod c -> View c () -> View c ()
nav f = tag "nav" (f . flexCol)


{- | Stack children on top of each other. Each child has the full width. See 'popup'

> stack id $ do
>   layer id "Background"
>   layer (bg Black . opacity 0.5) "Overlay"
-}
stack :: Mod c -> Layer c () -> View c ()
stack f (Layer children) = do
  tag "div" (f . container . absChildren) children
 where
  container =
    addClass $
      cls "stack"
        & prop @Text "position" "relative"
        & prop @Text "display" "grid"
        & prop @Text "overflow" "visible"
  absChildren =
    addClass $
      Class absSelector mempty
        & prop @Text "grid-area" "1 / 1"
        & prop @Text "min-height" "fit-content"
  absSelector = (selector "abs-childs"){child = Just AllChildren}


newtype Layer c a = Layer (View c a)
  deriving newtype (Functor, Applicative, Monad)


-- | A normal layer contributes to the size of the parent. See 'stack'
layer :: Mod c -> View c () -> Layer c ()
layer f cnt = Layer $ do
  el (flexCol . f) cnt


{- | This 'layer' is not included in the 'stack' size, and covers content outside of it. If used outside of stack, the popup is offset from the entire page.

> stack id $ do
>   layer id $ input (value "Autocomplete Box")
>   layer (popup (TRBL 50 0 0 0)) $ do
>     el_ "Item 1"
>     el_ "Item 2"
>     el_ "Item 3"
> el_ "This is covered by the menu"
-}
popup :: Sides Length -> Mod c
popup sides =
  position Absolute . offset sides


-- | Hide an element. See 'display'
hide :: Mod c
hide = display None


-- | Set container to be a row. Favor 'Web.View.Layout.row' when possible
flexRow :: Mod c
flexRow =
  addClass $
    cls "row"
      & prop @Text "display" "flex"
      & prop @Text "flex-direction" "row"


-- | Set container to be a column. Favor 'Web.View.Layout.col' when possible
flexCol :: Mod c
flexCol =
  addClass $
    cls "col"
      & prop @Text "display" "flex"
      & prop @Text "flex-direction" "column"


-- | Cut off the contents of the element
truncate :: Mod c
truncate =
  addClass $
    cls "truncate"
      & prop @Text "white-space" "nowrap"
      & prop @Text "overflow" "hidden"
      & prop @Text "text-overflow" "ellipsis"
