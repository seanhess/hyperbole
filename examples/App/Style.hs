module App.Style where

import Example.Colors
import Web.Atomic.CSS

-- btn :: (Styleable h) => CSS h -> CSS h
-- btn = btn' Primary
--
-- btn' :: (Styleable h) => AppColor -> CSS h -> CSS h
-- btn' clr =
--   bg clr
--     . hover (bg (hovClr clr))
--     . color (txtClr clr)
--     . pad 10
--     . shadow ()
--     . rounded 3
--  where
--   hovClr Primary = PrimaryLight
--   hovClr c = c
--   txtClr _ = White

btnLight :: (Styleable h) => CSS h -> CSS h
btnLight =
  base
    . border 2
    . borderColor Secondary
    . color Secondary
    . hover (borderColor SecondaryLight . color SecondaryLight)
 where
  base = pad (XY 15 8)

h1 :: (Styleable h) => CSS h -> CSS h
h1 = bold . fontSize 32

invalid :: (Styleable h) => CSS h -> CSS h
invalid = color Danger

success :: (Styleable h) => CSS h -> CSS h
success = color Success

link :: (Styleable h) => CSS h -> CSS h
link = color Primary . underline

input :: (Styleable h) => CSS h -> CSS h
input = border 1 . pad 8

strikethrough :: (Styleable h) => CSS h -> CSS h
strikethrough =
  utility "strike" ["text-decoration" :. "line-through"]

uppercase :: (Styleable h) => CSS h -> CSS h
uppercase = utility "upper" ["text-transform" :. "uppercase"]
