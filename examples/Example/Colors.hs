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
  deriving (Show, Read, Generic, ToJSON, FromJSON)

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
  colorValue Danger = "#ef1509"
  colorValue Warning = "#e1c915"
