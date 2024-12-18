module Web.Hyperbole.View
  ( hyper
  , module Web.Hyperbole.View.Element
  , module Web.Hyperbole.View.Event
  , module Web.View
  ) where

import Web.Hyperbole.HyperView (hyper)
import Web.Hyperbole.View.Element
import Web.Hyperbole.View.Event
import Web.View hiding (Query, Segment, button, cssResetEmbed, form, input, label)

