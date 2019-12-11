-- |
-- Module      :  CrossedWireSpec
-- Description :  Runs test for Day 3 challenge
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module CrossedWiresSpec where

import           CrossedWires
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Calculate closest intersection"
    $          context "when given a path [R75,D30,R83,U83,L12,D49,R71,U7,L72]"
    $          context "and another path [U62,R66,U55,R34,D71,R55,D58,R83]"
    $          it "the shortest distance from orign and intersection is 159"
    $          findClosest [R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72]
                           [U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83]
    `shouldBe` 159
