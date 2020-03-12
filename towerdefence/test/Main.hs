{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Holmes
import Data.JoinSemilattice.Intersect (fromList, singleton)
import Data.Propagator (lift)
import Test.Hspec (describe, hspec, it, shouldBe)
import Tower
import TowerIntersect

main :: IO ()
main = hspec do
  describe "figuring out that bounded -1 issue" do
    it "should return a single element" do
      let cfg = 1 `from` [1] :: Config Holmes (Defined Int)
      let prop _ = true
      result <- cfg `satisfying` prop
      result `shouldBe` Just [Exactly 1]
    it "should be happy with 1 >= 1" do
      let cfg = 1 `from` [1] :: Config Holmes (Intersect Val4)
          one = 1 :: Val4
          prop _ = lift (singleton one) .>= lift (singleton one)
      result <- cfg `satisfying` prop
      result `shouldBe` Just [fromList [one]]
    it "shouldn't cause an exception" do
      let cfg = 1 `from` [1 .. 4] :: Config Holmes (Intersect Val4)
          prop _ = lift (singleton 0) .>= lift (singleton (1 :: Val4))
      result <- cfg `satisfying` prop
      result `shouldBe` Nothing
