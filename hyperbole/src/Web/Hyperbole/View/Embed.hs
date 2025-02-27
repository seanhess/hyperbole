{-# LANGUAGE TemplateHaskell #-}

module Web.Hyperbole.View.Embed
  ( cssResetEmbed
  , cssResetLink
  , scriptEmbed
  )
where

import Data.ByteString
import Data.FileEmbed
import Web.View.Reset


scriptEmbed :: ByteString
scriptEmbed = $(embedFile "embed/hyperbole.js")
