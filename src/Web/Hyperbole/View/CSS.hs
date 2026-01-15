module Web.Hyperbole.View.CSS where

import Web.Atomic.CSS


{- | Apply CSS only when a request is in flight. See [Example.Page.Contact](https://docs.hyperbole.live/contacts/1)

@
#EMBED Example.Contact contactEditView
@
-}
whenLoading :: (Styleable h) => (CSS h -> CSS h) -> CSS h -> CSS h
whenLoading = do
  descendentOf "hyp-loading"


disabled :: (Styleable h) => CSS h -> CSS h
disabled =
  utility
    "disabled"
    [ "opacity" :. "0.5"
    , "pointer-events" :. "none"
    ]


loading :: (Styleable h) => CSS h -> CSS h
loading = whenLoading disabled
