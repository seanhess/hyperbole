module Web.Hyperbole.View.Render
  ( renderText
  , renderLazyByteString
  ) where

import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Web.Atomic.Render qualified as Atomic
import Web.Hyperbole.View.Types (View, runView)


renderText :: View () () -> Text
renderText = Atomic.renderText . runView ()


renderLazyByteString :: View () () -> BL.ByteString
renderLazyByteString = Atomic.renderLazyByteString . runView ()
