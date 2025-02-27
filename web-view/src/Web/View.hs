{- |
Module:      Web.View
Copyright:   (c) 2023 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

Type-safe HTML and CSS with intuitive layout and composable styles. Inspired by Tailwindcss and Elm-UI
-}
module Web.View
  ( -- * How to use this library
    -- $use

    -- ** Rendering 'View's
    renderText
  , renderLazyText
  , renderLazyByteString

    -- ** Full HTML Documents
    -- $documents
  , module Web.View.Reset

    -- * Views
  , View

    -- ** Mods
  , Mod

    -- * Elements
  , el
  , el_

    -- ** Layout
  , layout
  , root
  , col
  , row
  , space
  , nav
  , stack
  , Layer
  , layer
  , popup
  , scroll
  , grow
  , flexRow
  , flexCol
  , hide
  , truncate

    -- ** Content
  , text
  , raw
  , none
  , pre
  , code

    -- ** Inputs
  , form
  , input
  , name
  , value
  , label
  , link
  , button

    -- ** Lists
  , ol
  , ul
  , li

    -- ** Tables
  , table
  , tcol
  , th
  , td
  , TableHead
  , TableColumn

    -- ** Document Metadata
  , script
  , style
  , stylesheet

    -- * CSS Modifiers
  , width
  , height
  , minWidth
  , minHeight
  , pad
  , gap
  , opacity
  , shadow
  , Shadow
  , Inner (..)
  , rounded
  , fontSize
  , color
  , bg
  , bold
  , italic
  , underline
  , border
  , borderColor
  , pointer
  , position
  , Position (..)
  , zIndex
  , offset
  , textAlign
  , Align (..)
  , list
  , ListType (..)
  , display
  , Display (..)
  , transition
  , TransitionProperty (..)
  , Ms
  , flexWrap
  , textWrap
  , FlexWrap (..)
  , TextWrap
  , Wrap (..)

    -- ** Selector States
  , hover
  , active
  , even
  , odd
  , media
  , Media (..)
  , parent

    -- * View Context
  , context
  , addContext

    -- * Creating New Elements and Modifiers
  , tag
  , att

    -- * Types
  , Sides (..)
  , PxRem
  , Length (..)
  , ToColor (..)
  , HexColor (..)
  , None (..)
  , Attributes

    -- * Url
  , module Web.View.Types.Url
  , Query
  ) where

import Network.HTTP.Types (Query)
import Web.View.Element
import Web.View.Layout
import Web.View.Render
import Web.View.Reset
import Web.View.Style
import Web.View.Types
import Web.View.Types.Url
import Web.View.View
import Prelude hiding (even, head, odd, truncate)


{- $use

Create styled `View's using composable Haskell functions

> myView :: View ctx ()
> myView = col (gap 10) $ do
>  el (bold . fontSize 32) "My page"
>  button (border 1) "Click Me"

This represents an HTML fragment with embedded CSS definitions

> <style type='text/css'>
> .bold { font-weight:bold }
> .brd-1 { border:1px; border-style:solid }
> .col { display:flex; flex-direction:column }
> .fs-32 { font-size:2.0rem }
> .gap-10 { gap:0.625rem }
> </style>
>
> <div class='col gap-10'>
>   <div class='bold fs-32'>My page</div>
>   <button class='brd-1'>Click Me</button>
> </div>

Leverage the full power of Haskell functions for reuse, instead of relying on CSS.

> header = bold
> h1 = header . fontSize 32
> h2 = header . fontSize 24
> page = gap 10
>
> myView = col page $ do
>   el h1 "My Page"
> ...

This approach is inspired by Tailwindcss' [Utility Classes](https://tailwindcss.com/docs/utility-first)
-}


{- $documents

Create a full HTML document by embedding the view and 'cssResetEmbed'

> import Data.String.Interpolate (i)
> import Web.View
>
> toDocument :: Text -> Text
> toDocument content =
>   [i|<html>
>     <title>My Website</title>
>     <head><style type="text/css">#{cssResetEmbed}</style></head>
>     <body>#{content}</body>
>   </html>|]
>
> myDocument :: Text
> myDocument = toDocument $ renderText myView
-}
