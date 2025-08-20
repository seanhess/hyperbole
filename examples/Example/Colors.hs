module Example.Colors where

import Web.Atomic.CSS
import Web.Hyperbole

data AppColor
  = White
  | Light
  | GrayLight
  | GrayDark
  | Dark
  | DarkHighlight
  | Success
  | Danger
  | Warning
  | Primary
  | PrimaryLight
  | Secondary
  | SecondaryLight
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToParam, FromParam)

instance Default AppColor where
  def = White

instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Light = "#F2F2F3"
  colorValue GrayLight = "#E3E5E9"
  colorValue GrayDark = "#2С3С44"
  -- colorValue Dark = "#2E3842" -- "#232C41"
  colorValue Dark = "#121726" -- "#232C41"
  colorValue DarkHighlight = "#343945" -- "#232C41"
  colorValue Primary = "#4171b7"
  colorValue PrimaryLight = "#6D9BD3"
  colorValue Secondary = "#5D5A5C"
  colorValue SecondaryLight = "#9D999C"
  -- colorValue Success = "67C837"
  colorValue Success = "#149e5a"
  colorValue Danger = lightRed
  colorValue Warning = "#e1c915"

lightRed :: HexColor
lightRed = HexColor "#EC6458"

darkRed :: HexColor
darkRed = HexColor "#722C2A"

midRed :: HexColor
midRed = HexColor "#A03F38"

cyan :: HexColor
cyan = "#0FF"
