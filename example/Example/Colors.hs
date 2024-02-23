module Example.Colors where

import Data.String.Conversions
import Text.Read (readMaybe)
import Web.HttpApiData
import Web.Hyperbole


data AppColor
  = White
  | Light
  | GrayLight
  | GrayDark
  | Dark
  | Success
  | Error
  | Warning
  | Primary
  | PrimaryLight
  | Secondary
  | SecondaryLight
  deriving (Show, Read, Param)
instance ToHttpApiData AppColor where
  toQueryParam c = cs (show c)
instance FromHttpApiData AppColor where
  parseQueryParam t = do
    case readMaybe (cs t) of
      Nothing -> Left $ "Invalid AppColor: " <> t
      (Just c) -> pure c


instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Light = "#F2F2F3"
  colorValue GrayLight = "#E3E5E9"
  colorValue GrayDark = "#2С3С44"
  colorValue Dark = "#2E3842" -- "#232C41"
  colorValue Primary = "#2C74BB"
  colorValue PrimaryLight = "#3281cf"
  colorValue Secondary = "#5CADDB"
  colorValue SecondaryLight = "#8CFDAB"
  -- colorValue Success = "67C837"
  colorValue Success = "#D5E6DE"
  colorValue Error = "#F3D8DA"
  colorValue Warning = "#FDF3D1"
