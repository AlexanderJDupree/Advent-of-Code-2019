-- |
-- Module      :  FuelCountSpec
-- Description :  Runs test for Day 1 challenge
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module FuelCountSpec where

import           FuelCount
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FuelCount"
    $          context "when given 12"
    $          it "fuel count is 2"
    $          fuelCount 12
    `shouldBe` 2

  describe "FuelCount"
    $          context "when given 14"
    $          it "fuel count is 2"
    $          fuelCount 12
    `shouldBe` 2

  describe "FuelCount"
    $          context "when given 1969"
    $          it "fuel count is 654"
    $          fuelCount 1969
    `shouldBe` 654

  describe "FuelCount'"
    $          context "recursively applies fuelCount to the mass of the fuel"
    $          it "returns 966 when given 1969"
    $          fuelCount' 1969
    `shouldBe` 966

  describe "FuelCount'"
    $          context "recursively applies fuelCount to the mass of the fuel"
    $          it "returns 2 when given 14"
    $          fuelCount' 14
    `shouldBe` 2

  describe "FuelCount'"
    $          context "recursively applies fuelCount to the mass of the fuel"
    $          it "returns 50346 when given 100756"
    $          fuelCount' 100756
    `shouldBe` 50346
