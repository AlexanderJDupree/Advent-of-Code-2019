-- |
-- Module      :  Main
-- Description :  Entry point for Advent of Code 2019
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module Main where

import           FuelCount                     as D1
import           IntCode                       as D2
import           CrossedWires                  as D3
import           SecureContainer               as D4

main :: IO ()
main = do
  D1.answer
  D2.answer
  D3.answer
  D4.answer
