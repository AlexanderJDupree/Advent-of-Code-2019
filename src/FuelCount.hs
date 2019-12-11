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
  )
where

fuelCount :: Integral a => a -> a
fuelCount mass = (mass `div` 3) - 2

fuelCount' :: Integral a => a -> a
fuelCount' mass | mass > 0  = res + (fuelCount' res)
                | otherwise = 0
  where res = if fuelCount mass >= 0 then fuelCount mass else 0
