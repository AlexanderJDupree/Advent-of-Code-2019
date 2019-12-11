-- |
-- Module      :  IntCodeSpec
-- Description :  Runs test for Day 2 challenge
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module IntCodeSpec where

import           IntCode
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runIntCode"
    $          context "when given [1,0,0,0,99]"
    $          it "returns [2,0,0,0,99]"
    $          run 0 (readIntcode [1, 0, 0, 0, 99])
    `shouldBe` [2, 0, 0, 0, 99]

  describe "runIntCode"
    $          context "when given [2,3,0,3,99]"
    $          it "returns [2,3,0,6,99]"
    $          run 0 (readIntcode [2, 3, 0, 3, 99])
    `shouldBe` [2, 3, 0, 6, 99]

  describe "runIntCode"
    $          context "when given [2,4,4,5,99,0]"
    $          it "returns [2,4,4,5,99,9801]"
    $          run 0 (readIntcode [2, 4, 4, 5, 99, 0])
    `shouldBe` [2, 4, 4, 5, 99, 9801]

  describe "runIntCode"
    $          context "when given [1,1,1,4,99,5,6,0,99]"
    $          it "returns [30,1,1,4,2,5,6,0,99]"
    $          run 0 (readIntcode [1, 1, 1, 4, 99, 5, 6, 0, 99])
    `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]
