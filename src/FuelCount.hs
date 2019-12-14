-- |
-- Module      :  FuelCountSpec
-- Description :  Day 1 - Calcuate required fuel given a modules mass
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module FuelCount
  ( fuelCount
  , fuelCount'
  , answer
  )
where

import           System.IO

fuelCount :: Integral a => a -> a
fuelCount mass = (mass `div` 3) - 2

fuelCount' :: Integral a => a -> a
fuelCount' mass | mass > 0  = res + (fuelCount' res)
                | otherwise = 0
  where res = if fuelCount mass >= 0 then fuelCount mass else 0

answer :: IO ()
answer = do
  modules <- parseData "data/d1_input.txt"
  putStrLn $ "Day 1, Part 1 Answer: " ++ (show $ calculateFuel modules)
  putStrLn $ "Day 1, Part 2 Answer: " ++ (show $ calculateFuel' modules)
 where
  parseData file = readFile file >>= return <$> map (read) . lines
  calculateFuel modules = foldr ((+) . fuelCount) 0 modules
  calculateFuel' modules = foldr ((+) . fuelCount') 0 modules
