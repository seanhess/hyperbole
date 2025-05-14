{-# LANGUAGE TemplateHaskell #-}

module Web.Hyperbole.View.Embed
  ( Atomic.cssResetEmbed
  , Atomic.cssResetLink
  , scriptEmbed
  )
where

import Data.ByteString
import Data.FileEmbed
import Web.Atomic.CSS.Reset qualified as Atomic


scriptEmbed :: ByteString
scriptEmbed = $(embedFile "client/dist/hyperbole.js")
