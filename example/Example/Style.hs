module Example.Style where

import Example.Colors
import Web.View


btn :: Mod
btn = btn' Primary


btn' :: AppColor -> Mod
btn' clr = bg clr . hover (bg (hovClr clr)) . color (txtClr clr) . pad 10
 where
  hovClr Primary = PrimaryLight
  hovClr c = c
  txtClr _ = White


btnLight :: Mod
btnLight =
  base
    . border 2
    . borderColor Secondary
    . color Secondary
    . hover (borderColor SecondaryLight . color SecondaryLight)
 where
  base = pad (XY 15 8)


h1 :: Mod
h1 = bold . fontSize 32


invalid :: Mod
invalid = color Danger


success :: Mod
success = color Success


link :: Mod
link = color Primary
