module Web.Hyperbole.View
  ( hyper
  , autofocus
  , module Web.Hyperbole.View.Element
  , module Web.Hyperbole.View.Event
  , module Web.View
  , module Web.Hyperbole.View.Embed
  ) where

import Web.Hyperbole.HyperView (hyper)
import Web.Hyperbole.View.Element
import Web.Hyperbole.View.Embed
import Web.Hyperbole.View.Event
import Web.View hiding (Query, Segment, button, cssResetEmbed, form, input, label)


-- TODO: Remove once updating web-view to unreleased version
autofocus :: Mod id
autofocus = att "autofocus" ""
