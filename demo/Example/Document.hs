module Example.Document where

import Web.Hyperbole

main :: IO ()
main = do
  run 3000 $ liveApp (document documentHead) (runPage hello)

documentHead :: View DocumentHead ()
documentHead = do
  title "Best Website Ever"
  mobileFriendly
  style cssEmbed
  script' scriptEmbed
  stylesheet "/mysite.css"

hello :: Page es '[]
hello = do
  pure $ el "Hello World"
