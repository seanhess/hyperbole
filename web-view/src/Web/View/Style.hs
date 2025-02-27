{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Web.View.Style where

import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Web.View.Types


{- HLINT "HLint: shadows the existing binding" -}

-- * Styles


-- | Set to a specific width
width :: Length -> Mod c
width n =
  addClass $
    cls ("w" -. n)
      & prop "width" n
      & prop @Int "flex-shrink" 0


-- | Set to a specific height
height :: Length -> Mod c
height n =
  addClass $
    cls ("h" -. n)
      & prop "height" n
      & prop @Int "flex-shrink" 0


-- | Allow width to grow to contents but not shrink any smaller than value
minWidth :: Length -> Mod c
minWidth n =
  addClass $
    cls ("mw" -. n)
      & prop "min-width" n


-- | Allow height to grow to contents but not shrink any smaller than value
minHeight :: Length -> Mod c
minHeight n =
  addClass $
    cls ("mh" -. n)
      & prop "min-height" n


{- | Space surrounding the children of the element

To create even spacing around and between all elements:

> col (pad 10 . gap 10) $ do
>   el_ "one"
>   el_ "two"
>   el_ "three"
-}
pad :: Sides Length -> Mod c
pad (All n) =
  addClass $
    cls ("pad" -. n)
      & prop "padding" n
pad (Y n) =
  addClass $
    cls ("pady" -. n)
      & prop "padding-top" n
      & prop "padding-bottom" n
pad (X n) =
  addClass $
    cls ("padx" -. n)
      & prop "padding-left" n
      & prop "padding-right" n
pad (XY x y) = pad (TRBL y x y x)
pad (TRBL t r b l) =
  addClass $
    cls ("pad" -. t -. r -. b -. l)
      & prop "padding-top" t
      & prop "padding-right" r
      & prop "padding-bottom" b
      & prop "padding-left" l
pad (T x) = addClass $ cls ("padt" -. x) & prop "padding-top" x
pad (R x) = addClass $ cls ("padr" -. x) & prop "padding-right" x
pad (B x) = addClass $ cls ("padb" -. x) & prop "padding-bottom" x
pad (L x) = addClass $ cls ("padl" -. x) & prop "padding-left" x
pad (TR t r) = pad (TRBL t r 0 0)
pad (TL t l) = pad (TRBL t 0 0 l)
pad (BR b r) = pad (TRBL 0 r b 0)
pad (BL b l) = pad (TRBL 0 0 b l)


-- | The space between child elements. See 'pad'
gap :: Length -> Mod c
gap n = addClass $ cls ("gap" -. n) & prop "gap" n


fontSize :: Length -> Mod c
fontSize n = addClass $ cls ("fs" -. n) & prop "font-size" n


-- fontFamily :: Text -> Mod c
-- fontFamily t = cls1 $ Class ("font" -. n) [("font-family", pxRem n)]

{- | Add a drop shadow to an element

> input (shadow Inner) "Inset Shadow"
> button (shadow ()) "Click Me"
-}
shadow :: (Style Shadow a, ToClassName a) => a -> Mod c
shadow a =
  addClass $
    cls ("shadow" -. a)
      & prop "box-shadow" (styleValue @Shadow a)


-- "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)"

data Shadow
data Inner = Inner
  deriving (Show, ToClassName)


instance Style Shadow () where
  styleValue _ = "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1);"
instance Style Shadow None where
  styleValue _ = "0 0 #0000;"
instance Style Shadow Inner where
  styleValue _ = "inset 0 2px 4px 0 rgb(0 0 0 / 0.05);"


-- | Round the corners of the element
rounded :: Length -> Mod c
rounded n = addClass $ cls ("rnd" -. n) & prop "border-radius" n


-- | Set the background color. See 'Web.View.Types.ToColor'
bg :: (ToColor clr) => clr -> Mod ctx
bg c =
  addClass $
    cls ("bg" -. colorName c)
      & prop "background-color" (colorValue c)


-- | Set the text color. See 'Web.View.Types.ToColor'
color :: (ToColor clr) => clr -> Mod ctx
color c = addClass $ cls ("clr" -. colorName c) & prop "color" (colorValue c)


bold :: Mod c
bold = addClass $ cls "bold" & prop @Text "font-weight" "bold"


italic :: Mod c
italic = addClass $ cls "italic" & prop @Text "font-style" "italic"


underline :: Mod c
underline = addClass $ cls "underline" & prop @Text "text-decoration" "underline"


{- | Set the list style of an item

> ol id $ do
>   li (list Decimal) "First"
>   li (list Decimal) "Second"
>   li (list Decimal) "Third"
-}
list :: (ToClassName a, Style ListType a) => a -> Mod c
list a =
  addClass $
    cls ("list" -. a)
      & prop "list-style-type" (styleValue @ListType a)


data ListType
  = Decimal
  | Disc
  deriving (Show, ToClassName, ToStyleValue)
instance Style ListType ListType
instance Style ListType None


opacity :: Float -> Mod c
opacity n =
  addClass $
    cls ("opacity" -. n)
      & prop "opacity" n


{- | Set a border around the element

> el (border 1) "all sides"
> el (border (X 1)) "only left and right"
-}
border :: Sides PxRem -> Mod c
border (All p) =
  addClass $
    cls ("brd" -. p)
      & prop "border-width" p
      & prop @Text "border-style" "solid"
border (Y p) =
  addClass $
    cls ("brdy" -. p)
      & prop "border-top-width" p
      & prop "border-bottom-width" p
border (X p) =
  addClass $
    cls ("brdx" -. p)
      & prop "border-left-width" p
      & prop "border-right-width" p
border (XY x y) = border (TRBL y x y x)
border (TRBL t r b l) =
  addClass $
    cls ("brd" -. t -. r -. b -. l)
      & prop "border-top-width" t
      & prop "border-right-width" r
      & prop "border-bottom-width" b
      & prop "border-left-width" l
border (T x) = addClass $ cls ("brdt" -. x) & prop "border-top-width" x
border (R x) = addClass $ cls ("brdr" -. x) & prop "border-right-width" x
border (B x) = addClass $ cls ("brdb" -. x) & prop "border-bottom-width" x
border (L x) = addClass $ cls ("brdl" -. x) & prop "border-left-width" x
border (TR t r) = border (TRBL t r 0 0)
border (TL t l) = border (TRBL t 0 0 l)
border (BR b r) = border (TRBL 0 r b 0)
border (BL b l) = border (TRBL 0 0 b l)


-- | Set a border color. See 'Web.View.Types.ToColor'
borderColor :: (ToColor clr) => clr -> Mod ctx
borderColor c =
  addClass $
    cls ("brdc" -. colorName c)
      & prop "border-color" (colorValue c)


{- | Use a button-like cursor when hovering over the element

Button-like elements:

> btn = pointer . bg Primary . hover (bg PrimaryLight)
>
> options = row id $ do
>   el btn "Login"
>   el btn "Sign Up"
-}
pointer :: Mod c
pointer = addClass $ cls "pointer" & prop @Text "cursor" "pointer"


{- | Animate changes to the given property

> el (transition 100 (Height 400)) "Tall"
> el (transition 100 (Height 100)) "Small"
-}
transition :: Ms -> TransitionProperty -> Mod c
transition ms = \case
  (Height n) -> trans "height" n
  (Width n) -> trans "width" n
  (BgColor c) -> trans "background-color" c
  (Color c) -> trans "color" c
 where
  trans p val =
    addClass $
      cls ("t" -. val -. p -. ms)
        & prop "transition-duration" ms
        & prop "transition-property" p
        & prop p val


-- You MUST set the height/width manually when you attempt to transition it
data TransitionProperty
  = Width PxRem
  | Height PxRem
  | BgColor HexColor
  | Color HexColor
  deriving (Show)


textAlign :: Align -> Mod c
textAlign a =
  addClass $
    cls ("ta" -. a)
      & prop "text-align" a


-- | position:absolute, relative, etc. See 'Web.View.Layout.stack' and 'Web.View.Layout.popup'
position :: Position -> Mod c
position p = addClass $ cls (toClassName p) & prop "position" p


data Position
  = Absolute
  | Fixed
  | Sticky
  | Relative
  deriving (Show, ToClassName, ToStyleValue)


zIndex :: Int -> Mod c
zIndex n = addClass $ cls ("z" -. n) & prop "z-index" n


-- | Set top, bottom, right, and left. See 'Web.View.Layout.stack' and 'Web.View.Layout.popup'
offset :: Sides Length -> Mod c
offset sides = addClass (off sides)
 where
  off :: Sides Length -> Class
  off = \case
    All n -> off (TRBL n n n n)
    Y n -> off (XY 0 n)
    X n -> off (XY n 0)
    XY x y -> off (TRBL y x y x)
    TRBL t r b l ->
      cls ("pop" -. t -. r -. b -. l)
        & prop "top" t
        & prop "right" r
        & prop "bottom" b
        & prop "left" l
    T x -> cls ("popt" -. x) & prop "top" x
    R x -> cls ("popr" -. x) & prop "right" x
    B x -> cls ("popb" -. x) & prop "bottom" x
    L x -> cls ("popl" -. x) & prop "left" x
    TR t r ->
      cls ("poptr" -. t -. r)
        & prop "top" t
        & prop "right" r
    TL t l ->
      cls ("poptl" -. t -. l)
        & prop "top" t
        & prop "left" l
    BR b r ->
      cls ("popbr" -. b -. r)
        & prop "right" r
        & prop "bottom" b
    BL b l ->
      cls ("popbl" -. b -. l)
        & prop "bottom" b
        & prop "left" l


{- | Set container display

el (display None) "HIDDEN"
-}
display :: (Style Display a, ToClassName a) => a -> Mod c
display disp =
  addClass $
    cls ("disp" -. disp)
      & prop "display" (styleValue @Display disp)


data Display
  = Block
  deriving (Show, ToClassName, ToStyleValue)
instance Style Display Display
instance Style Display None


data Wrap
  = Wrap
  | NoWrap
  deriving (Show, ToClassName)
instance ToStyleValue Wrap where
  toStyleValue Wrap = "wrap"
  toStyleValue NoWrap = "nowrap"


data FlexWrap
  = WrapReverse
  deriving (Show, ToStyleValue)
instance Style FlexWrap FlexWrap
instance Style FlexWrap Wrap
instance ToClassName FlexWrap where
  toClassName WrapReverse = "rev"


flexWrap :: (Style FlexWrap a, ToClassName a, ToStyleValue a) => a -> Mod c
flexWrap w =
  addClass $
    cls ("fwrap" -. w)
      & prop "flex-wrap" w


data TextWrap


--   = Balance
--   | Pretty
--   | Stable
--   deriving (Show, ToStyleValue, ToClassName)
-- instance Style TextWrap TextWrap
instance Style TextWrap Wrap


textWrap :: (Style TextWrap a, ToClassName a, ToStyleValue a) => a -> Mod c
textWrap w =
  addClass $
    cls ("twrap" -. w)
      & prop "text-wrap" w


-- * Selector Modifiers


{- | Apply when hovering over an element

> el (bg Primary . hover (bg PrimaryLight)) "Hover"
-}
hover :: Mod c -> Mod c
hover = applyPseudo Hover


-- | Apply when the mouse is pressed down on an element
active :: Mod c -> Mod c
active = applyPseudo Active


-- | Apply to even-numbered children
even :: Mod c -> Mod c
even = applyPseudo Even


-- | Apply to odd-numbered children
odd :: Mod c -> Mod c
odd = applyPseudo Odd


{- | Apply when the Media matches the current window. This allows for responsive designs

> el (width 100 . media (MinWidth 800) (width 400))
>   "Big if window > 800"
-}
media :: Media -> Mod c -> Mod c
media m = mapModClass $ \c ->
  c
    { selector = addMedia c.selector
    }
 where
  addMedia :: Selector -> Selector
  addMedia Selector{..} = Selector{media = Just m, ..}


{- | Apply when the element is somewhere inside an anscestor.

For example, the HTMX library applies an "htmx-request" class to the body when a request is pending. We can use this to create a loading indicator

> el (pad 10) $ do
>   el (parent "htmx-request" flexRow . hide) "Loading..."
>   el (parent "htmx-request" hide . flexRow) "Normal Content"
-}
parent :: Text -> Mod c -> Mod c
parent p = mapModClass $ \c ->
  c
    { selector = addAncestor c.selector
    }
 where
  addAncestor :: Selector -> Selector
  addAncestor Selector{..} = Selector{ancestor = Just p, ..}


-- Add a pseudo-class like Hover to your style
applyPseudo :: Pseudo -> Mod c -> Mod c
applyPseudo ps = mapModClass $ \c ->
  c
    { selector = addToSelector c.selector
    }
 where
  addToSelector :: Selector -> Selector
  addToSelector Selector{..} = Selector{pseudo = Just ps, ..}


mapModClass :: (Class -> Class) -> Mod c -> Mod c
mapModClass fc fm as =
  -- apply the function to all classes added by the mod
  -- ignore
  let as' = fm $ Attributes [] []
   in as'
        { classes = as.classes <> fmap fc as'.classes
        , other = as.other <> as'.other
        }


{- | Setting the same property twice will result in only one of the classes being applied. It is not intuitive, as CSS rules dictate that the order of the class definitions determine precedence. You can mark a `Mod` as important to force it to apply
important :: Mod c -> Mod c
important =
  mapModClass $ \c ->
    c
      { important = True
      }
-}

-- * Creating New Styles


{- | Add a single class

> width :: PxRem -> Mod
> width n =
>   addClass
>     $ cls ("w" -. n)
>     & prop "width" n
>     & prop @Int "flex-shrink" 0
-}
addClass :: Class -> Mod c
addClass c attributes =
  Attributes
    { classes = M.insert c.selector c attributes.classes
    , other = attributes.other
    }


-- | Construct a class from a ClassName
cls :: ClassName -> Class
cls n = Class (selector n) []


{- | Construct a mod from a ClassName with no CSS properties. Convenience for situations where external CSS classes need to be referenced.

> el (extClass "btn" . extClass "btn-primary") "Click me!"
-}
extClass :: ClassName -> Mod c
extClass = addClass . cls


-- | Add a property to a class
prop :: (ToStyleValue val) => Name -> val -> Class -> Class
prop n v c =
  c{properties = M.insert n (toStyleValue v) c.properties}


-- | Hyphenate classnames
(-.) :: (ToClassName a) => ClassName -> a -> ClassName
(ClassName n) -. a =
  case toClassName a of
    "" -> ClassName n
    suffix -> (ClassName $ n <> "-") <> suffix


infixl 6 -.
