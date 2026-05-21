{-# LANGUAGE QuasiQuotes #-}

module Example.View.Loader where

import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Web.Atomic.CSS
import Web.Hyperbole

css :: ByteString
css =
  [i|
.loader {
  width: 24px;
  aspect-ratio: 1;
  --c: no-repeat linear-gradient(\#E44072 0 0);
  background: 
    var(--c) 0%   50%,
    var(--c) 50%  50%,
    var(--c) 100% 50%;
  background-size: 20% 100%;
  animation: l1 1s infinite linear;
}
@keyframes l1 {
  0%  {background-size: 20% 100%,20% 100%,20% 100%}
  33% {background-size: 20% 10% ,20% 100%,20% 100%}
  50% {background-size: 20% 100%,20% 10% ,20% 100%}
  66% {background-size: 20% 100%,20% 100%,20% 10% }
  100%{background-size: 20% 100%,20% 100%,20% 100%}
}
|]

loadingBars :: View c ()
loadingBars = el ~ cls "loader" $ none

loading :: View c ()
loading = do
  row ~ gap 10 . whenLoading flexRow . display None $ do
    loadingBars
    el "Loading..."
