module Example.Style.Cyber where

import Data.Text (Text, pack)
import Example.Colors
import Web.Atomic.CSS
import Web.Atomic.Types (Declaration (..), Property (..), style, (-.))
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

cyan :: HexColor
cyan = "#0FF"

red :: HexColor
red = HexColor "#EC6458"

-- toError :: Text -> View Body ()
-- toError msg = do
--   el ~ clip 10 . bg red . color (HexColor "#FFF") . uppercase . pad 10 $ do
--     text msg

uppercase :: (Styleable h) => CSS h -> CSS h
uppercase = utility "upper" ["text-transform" :. "uppercase"]

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
    . uppercase
 where
  txtClr _ = White

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

hovClr Primary = PrimaryLight
hovClr c = c

font :: (Styleable h) => CSS h -> CSS h
font = utility ("share-tech") ["font-family" :. "'Share Tech Mono'"]

cyberError :: View Body () -> View Body ()
cyberError inner = el ~ bg red . clip 10 . pad 10 . color White $ inner
