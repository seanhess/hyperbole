module Test.RouteSpec (spec) where

import Test.Syd
import Web.Hyperbole.Route


spec :: Spec
spec = do
  describe "Route" $ do
    -- description of which functions you are testing
    it "does what you want it to" $ -- sentence to describe what you expect to happen
      2 + 3 == (6 :: Int) -- Test code :: Int)
