module Web.Hyperbole.View.Render
  ( renderText
  , renderLazyByteString
  , renderBody
  ) where

import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Web.Atomic.Render qualified as Atomic
import Web.Hyperbole.Types.Response (Body (..))
import Web.Hyperbole.View.Types (View, execView)


renderText :: View () () -> Text
renderText = Atomic.renderText . execView () ()


renderLazyByteString :: View () () -> BL.ByteString
renderLazyByteString = Atomic.renderLazyByteString . execView () ()


renderBody :: View () () -> Body
renderBody v = Body $ renderLazyByteString v
