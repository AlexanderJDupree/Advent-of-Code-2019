-- |
-- Module      :  Main
-- Description :  Entry point for Advent of Code 2019
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module Main where

import           FuelCount
import           IntCode
import           CrossedWires
import           System.IO
import           Data.List.Split

type Set a = [a]

-- TODO: Combine file parsers into higher order function
parseInputFile :: FilePath -> IO [Integer]
parseInputFile file = readFile file >>= return <$> map (read) . lines

parseIntCode :: FilePath -> IO [Int]
parseIntCode file = readFile file >>= return <$> map (read) . (splitOn ",")

parseMovements :: FilePath -> IO [[Movement]]
parseMovements file = readFile file >>= return <$> map (parseLine) . lines
 where
  parseLine = map (readMovement) . (splitOn ",")
  readMovement str = case str of
    'R' : rest -> R (read rest :: Float)
    'L' : rest -> L (read rest :: Float)
    'U' : rest -> U (read rest :: Float)
    'D' : rest -> D (read rest :: Float)

computePair :: Int -> [Int] -> (Int, Int)
computePair target d2_data = computePair'
  [ (n, v) | n <- [0 .. 99], v <- [0 .. 99] ]
 where
  computePair' []       = (-1, -1)
  computePair' (x : xs) = case evaluate target x d2_data of
    True  -> x
    False -> computePair' xs

evaluate :: Int -> (Int, Int) -> [Int] -> Bool
evaluate target x d2_data = (head $ run 0 $ prepareData x) == target
 where
  prepareData (n, v) = readIntcode $ head d2_data : [n, v] ++ (drop 3 d2_data)

main :: IO ()
main = do
  modules        <- parseInputFile "data/d1_input.txt"
  d2_data        <- parseIntCode "data/d2_input.txt"
  [path1, path2] <- parseMovements "data/d3_input.txt"
  putStrLn $ "Day 1, Part 1 Answer: " ++ (show $ calculateFuel modules)
  putStrLn $ "Day 1, Part 2 Answer: " ++ (show $ calculateFuel' modules)
  putStrLn
    $  "Day 2, Part 1 Answer: "
    ++ (show $ head $ run 0 $ readIntcode d2_data)
  putStrLn $ "Day 2, Part 2 Answer: " ++ (show $ computePair 19690720 d2_data)
  putStrLn $ "Day 3, Part 1 Answer: " ++ (show $ calcMinDistance path1 path2)
  putStrLn $ "Day 3, Part 2 Answer: " ++ (show $ calcMinSteps path1 path2)
 where
  calculateFuel modules = foldr ((+) . fuelCount) 0 modules
  calculateFuel' modules = foldr ((+) . fuelCount') 0 modules
