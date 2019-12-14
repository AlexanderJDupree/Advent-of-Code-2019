-- |
-- Module      :  SecureContainerSpec
-- Description :  Runs test for Day 4 challenge
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module SecureContainerSpec where

import           SecureContainer
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "possiblePasswords"
    $          context "when given a range of input"
    $          it "generates a list of possible password"
    $          (111122 `elem` possiblePasswords 100000 600000)
    `shouldBe` True

  describe "possiblePasswords"
    $          context "when given a range of input"
    $          it "generates a list of possible password"
    $          (223450 `elem` possiblePasswords 100000 600000)
    `shouldBe` False
