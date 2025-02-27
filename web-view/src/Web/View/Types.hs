{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Web.View.Types where

import Data.Char (toLower)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Text.Casing (kebab)


data Content
  = Node Element
  | Text Text
  | -- | Raw embedded HTML or SVG. See 'Web.View.Element.raw'
    Raw Text
  deriving (Show, Eq)


-- | A single HTML tag. Note that the class attribute is stored separately from the rest of the attributes to make adding styles easier
data Element = Element
  { inline :: Bool
  , name :: Name
  , attributes :: Attributes ()
  , children :: [Content]
  }
  deriving (Show, Eq)


-- | Construct an Element
element :: Name -> Attributes c -> [Content] -> Element
element n atts =
  Element False n (stripContext atts)


-- | Internal. Convert an Attributes to any context
stripContext :: Attributes a -> Attributes b
stripContext (Attributes cls other) = Attributes cls other


-- | The Attributes for an 'Element'. Classes are merged and managed separately from the other attributes.
data Attributes c = Attributes
  { classes :: CSS
  , other :: Map Name AttValue
  }
  deriving (Show, Eq)


instance Semigroup (Attributes c) where
  a1 <> a2 = Attributes (a1.classes <> a2.classes) (a1.other <> a2.other)
instance Monoid (Attributes c) where
  mempty = Attributes mempty mempty
type Attribute = (Name, AttValue)
type Name = Text
type AttValue = Text


-- * Attribute Modifiers


{- | Element functions expect a modifier function as their first argument. These can add attributes and classes. Combine multiple `Mod`s with (`.`)

> userEmail :: User -> View c ()
> userEmail user = input (fontSize 16 . active) (text user.email)
>   where
>     active = isActive user then bold else id

If you don't want to specify any attributes, you can use `id`

> plainView :: View c ()
> plainView = el id "No styles"
-}
type Mod (context :: Type) = Attributes context -> Attributes context


-- * Atomic CSS


-- TODO: document atomic CSS here?

-- | All the atomic classes used in a 'Web.View.View'
type CSS = Map Selector Class


-- | Atomic classes include a selector and the corresponding styles
data Class = Class
  { selector :: Selector
  , properties :: Styles
  }
  deriving (Show, Eq)


-- | The styles to apply for a given atomic 'Class'
type Styles = Map Name StyleValue


-- | A parent selector limits the selector to only apply when a descendent of the parent in question
type Ancestor = Text


-- | A child selector limits
data ChildCombinator
  = AllChildren
  | ChildWithName Text
  deriving (Show, Eq, Ord)


instance IsString ChildCombinator where
  fromString s = ChildWithName (fromString s)


-- | The selector to use for the given atomic 'Class'
data Selector = Selector
  { media :: Maybe Media
  , ancestor :: Maybe Ancestor
  , child :: Maybe ChildCombinator
  , pseudo :: Maybe Pseudo
  , className :: ClassName
  }
  deriving (Eq, Ord, Show)


instance IsString Selector where
  fromString s = selector (fromString s)


-- | Create a 'Selector' given only a 'ClassName'
selector :: ClassName -> Selector
selector c =
  Selector
    { pseudo = Nothing
    , ancestor = Nothing
    , child = Nothing
    , media = Nothing
    , className = c
    }


-- | A class name
newtype ClassName = ClassName
  { text :: Text
  }
  deriving newtype (Eq, Ord, Show, Monoid, Semigroup)


instance IsString ClassName where
  fromString s = ClassName $ pack s


-- | Create a class name, escaping special characters
className :: Text -> ClassName
className = ClassName . T.toLower . T.map noDot
 where
  noDot '.' = '-'
  noDot c = c


-- | Convert a type into a className segment to generate unique compound style names based on the value
class ToClassName a where
  toClassName :: a -> ClassName
  default toClassName :: (Show a) => a -> ClassName
  toClassName = className . T.pack . show


instance ToClassName Int
instance ToClassName Text where
  toClassName = className
instance ToClassName Float where
  toClassName f = className $ pack $ showFFloat (Just 3) f ""
instance ToClassName () where
  toClassName _ = ""


{- | Psuedos allow for specifying styles that only apply in certain conditions. See `Web.View.Style.hover` etc

> el (color Primary . hover (color White)) "hello"
-}
data Pseudo
  = Hover
  | Active
  | Even
  | Odd
  deriving (Show, Eq, Ord)


-- | The value of a css style property
newtype StyleValue = StyleValue String
  deriving newtype (IsString, Show, Eq, Monoid, Semigroup)


-- | Use a type as a css style property value
class ToStyleValue a where
  toStyleValue :: a -> StyleValue
  default toStyleValue :: (Show a) => a -> StyleValue
  toStyleValue = StyleValue . kebab . show


instance ToStyleValue String where
  toStyleValue = StyleValue


instance ToStyleValue Text where
  toStyleValue = StyleValue . unpack


instance ToStyleValue Int


instance ToStyleValue Float where
  -- this does not convert to a percent, just a ratio
  toStyleValue n = StyleValue $ showFFloat (Just 2) n ""


instance ToStyleValue StyleValue where
  toStyleValue = id


-- | Convert a type to a prop name
class ToProp a where
  toProp :: a -> Name
  default toProp :: (Show a) => a -> Name
  toProp = pack . kebab . show


data Length
  = PxRem PxRem
  | Pct Float
  deriving (Show)


instance ToClassName Length where
  toClassName (PxRem p) = toClassName p
  toClassName (Pct p) = toClassName p


-- | Px, converted to Rem. Allows for the user to change the document font size and have the app scale accordingly. But allows the programmer to code in pixels to match a design
newtype PxRem = PxRem' Int
  deriving newtype (Show, ToClassName, Num, Eq, Integral, Real, Ord, Enum)


instance Num Length where
  -- only support numeric literals
  a + _ = a
  a * _ = a
  abs (PxRem a) = PxRem (abs a)
  abs (Pct a) = Pct (abs a)
  signum (PxRem a) = PxRem (signum a)
  signum (Pct a) = Pct (signum a)
  negate (PxRem a) = PxRem (negate a)
  negate (Pct a) = Pct (negate a)
  fromInteger n = PxRem (fromInteger n)


instance ToStyleValue PxRem where
  toStyleValue (PxRem' 0) = "0px"
  toStyleValue (PxRem' 1) = "1px"
  toStyleValue (PxRem' n) = StyleValue $ show ((fromIntegral n :: Float) / 16.0) <> "rem"


instance ToStyleValue Length where
  toStyleValue (PxRem p) = toStyleValue p
  toStyleValue (Pct n) = StyleValue $ showFFloat (Just 1) (n * 100) "" <> "%"


-- | Milliseconds, used for transitions
newtype Ms = Ms Int
  deriving (Show)
  deriving newtype (Num, ToClassName)


instance ToStyleValue Ms where
  toStyleValue (Ms n) = StyleValue $ show n <> "ms"


-- | Media allows for responsive designs that change based on characteristics of the window. See [Layout Example](https://github.com/seanhess/web-view/blob/master/example/Example/Layout.hs)
data Media
  = MinWidth Int
  | MaxWidth Int
  deriving (Eq, Ord, Show)


{- | Options for styles that support specifying various sides. This has a "fake" Num instance to support literals

> border 5
> border (X 2)
> border (TRBL 0 5 0 0)
-}
data Sides a
  = All a
  | TRBL a a a a
  | X a
  | Y a
  | XY a a
  | T a
  | R a
  | B a
  | L a
  | TR a a
  | TL a a
  | BR a a
  | BL a a


-- Num instance is just to support literals
instance (Num a) => Num (Sides a) where
  a + _ = a
  a * _ = a
  abs a = a
  negate a = a
  signum a = a
  fromInteger n = All (fromInteger n)


-- | Element's attributes do not include class, which is separated. FlatAttributes generate the class attribute and include it
newtype FlatAttributes = FlatAttributes {attributes :: Map Name AttValue}
  deriving (Generic)


-- ** Colors


{- | ToColor allows you to create a type containing your application's colors:

> data AppColor
>   = White
>   | Primary
>   | Dark
>
> instance ToColor AppColor where
>   colorValue White = "#FFF"
>   colorValue Dark = "#333"
>   colorValue Primary = "#00F"
>
> hello :: View c ()
> hello = el (bg Primary . color White) "Hello"
-}
class ToColor a where
  colorValue :: a -> HexColor
  colorName :: a -> Text
  default colorName :: (Show a) => a -> Text
  colorName = T.toLower . pack . show


-- | Hexidecimal Color. Can be specified with or without the leading '#'. Recommended to use an AppColor type instead of manually using hex colors. See 'Web.View.Types.ToColor'
newtype HexColor = HexColor Text
  deriving (Show)


instance ToColor HexColor where
  colorValue c = c
  colorName (HexColor a) = T.dropWhile (== '#') a


instance ToStyleValue HexColor where
  toStyleValue (HexColor s) = StyleValue $ "#" <> unpack (T.dropWhile (== '#') s)


instance IsString HexColor where
  fromString = HexColor . T.dropWhile (== '#') . T.pack


instance ToClassName HexColor where
  toClassName = className . colorName


data Align
  = AlignCenter
  | AlignLeft
  | AlignRight
  | AlignJustify
  deriving (Show, ToClassName)
instance ToStyleValue Align where
  toStyleValue a = StyleValue $ map toLower $ drop 5 $ show a


data None = None
  deriving (Show, ToClassName, ToStyleValue)


-- uniquely set the style value based on the style
class Style cls value where
  styleValue :: value -> StyleValue
  default styleValue :: (ToStyleValue value) => value -> StyleValue
  styleValue = toStyleValue

class ToClass cls value where
  toClass :: value -> Class
