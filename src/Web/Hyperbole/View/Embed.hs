{-# LANGUAGE TemplateHaskell #-}

module Web.Hyperbole.View.Embed
  ( cssEmbed
  , scriptEmbed
  , scriptLiveReload
  )
where

import Data.ByteString
import Data.FileEmbed
import Web.Atomic.CSS.Reset qualified as Atomic


scriptEmbed :: ByteString
scriptEmbed = $(embedFile "client/dist/hyperbole.js")


scriptLiveReload :: ByteString
scriptLiveReload = $(embedFile "client/util/live-reload.js")


cssEmbed :: ByteString
cssEmbed =
  Atomic.cssResetEmbed
    <> "\n"
    <> intercalate
      "\n"
      ["form, label { display: flex; flex-direction: column } "]
