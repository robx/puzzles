{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Holmes
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec do
  describe "figuring out that bounded -1 issue" do
    it "should return a single element" do
      let cfg = 1 `from` [1] :: Config Holmes (Defined Int)
      let prop _ = true
      result <- cfg `satisfying` prop
      result `shouldBe` Just [Exactly 1]
