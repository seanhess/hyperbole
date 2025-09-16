module Example.Style.Cyber where

import Data.Text (Text, pack)
import Example.Colors
import Web.Atomic.CSS
import Web.Atomic.Types (style, (-.))
import Web.Hyperbole hiding (style)
import Web.Hyperbole.Types.Response

clip :: (Styleable h) => PxRem -> CSS h -> CSS h
clip size =
  utility
    ("clip-br" -. size)
    ["clip-path" :. ("polygon(0 0, 100% 0, 100% calc(100% - " <> style size <> "), calc(100% - " <> style size <> ") 100%, 0 100%);")]

textShadow :: (Styleable h) => CSS h -> CSS h
textShadow =
  utility
    "text-shadow"
    ["text-shadow" :. "0 0 4px #0ff, 0 0 8px #0ff"]

dataFeature :: (Styleable h) => CSS h -> CSS h
dataFeature =
  bold . fontSize 48 . border 1 . pad (XY 20 0) . font . textAlign AlignCenter

btn :: (Styleable h) => CSS h -> CSS h
btn = btn' Primary

btn' :: (Styleable h) => AppColor -> CSS h -> CSS h
btn' clr =
  bgAnimated
    . bgGradient clr
    . hover bgzero
    . font
    . color (txtClr clr)
    . pad 10
    . clip 10
    . shadow ()
 where
  txtClr _ = White

btnLight :: (Styleable h) => CSS h -> CSS h
btnLight =
  base
    . border 2
    . borderColor Secondary
    . font
    . color Secondary
    . hover (borderColor SecondaryLight . color SecondaryLight)
 where
  base = pad (XY 15 8)

bgAnimated :: (Styleable h) => CSS h -> CSS h
bgAnimated =
  utility
    "bg-anim"
    [ "background-size" :. "200% 100%"
    , "background-position" :. "100% 0"
    , "transition" :. "background-position 0.1s linear"
    ]

bgzero :: (Styleable h) => CSS h -> CSS h
bgzero =
  utility "bg0" ["background-position" :. "0 0"]

bgGradient :: (Styleable h) => AppColor -> CSS h -> CSS h
bgGradient clr =
  utility
    ("bg-grad" -. pack (show clr))
    ["background-image" :. ("linear-gradient(90deg, " <> style (colorValue (hovClr clr)) <> " 0 50%, " <> style (colorValue clr) <> " 50% 100%)")]

hovClr :: AppColor -> AppColor
hovClr Primary = PrimaryLight
hovClr c = c

font :: (Styleable h) => CSS h -> CSS h
font = utility ("share-tech") ["font-family" :. "'Share Tech Mono'"]

cyberError :: View Body () -> View Body ()
cyberError inner =
  el ~ wipeIn . border (T 4) . borderColor lightRed $ do
    el ~ bg midRed . clip 10 . pad 10 . color White $
      inner
 where
  -- requires @keyframes wipeIn
  wipeIn :: (Styleable h) => CSS h -> CSS h
  wipeIn = utility "wipe-in" ["animation" :. "wipeIn 0.5s steps(20, end) forwards"]

glitch :: Text -> View Body ()
glitch msg =
  el ~ cls "glitch" @ att "data-text" msg $ text msg
