module Example.Style where

import Data.Function ((&))
import Data.Text (Text)
import Example.Colors
import Web.View
import Web.View.Style (addClass, cls, prop)

btn :: Mod id
btn = btn' Primary

btn' :: AppColor -> Mod id
btn' clr =
  bg clr
    . hover (bg (hovClr clr))
    . color (txtClr clr)
    . pad 10
    . shadow ()
    . rounded 3
 where
  hovClr Primary = PrimaryLight
  hovClr c = c
  txtClr _ = White

btnLight :: Mod id
btnLight =
  base
    . border 2
    . borderColor Secondary
    . color Secondary
    . hover (borderColor SecondaryLight . color SecondaryLight)
 where
  base = pad (XY 15 8)

h1 :: Mod id
h1 = bold . fontSize 32

invalid :: Mod id
invalid = color Danger

success :: Mod id
success = color Success

link :: Mod id
link = color Primary

input :: Mod id
input = border 1 . pad 8

code :: Mod id
code = bg Light . pad 10 . fontSize 12

strikethrough :: Mod id
strikethrough =
  addClass $
    cls "strike"
      & prop @Text "text-decoration" "line-through"
