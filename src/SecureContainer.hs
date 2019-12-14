-- |
-- Module      :  SecureContainer
-- Description :  Day 4 - Find range of passwords that may open a secure container
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module SecureContainer
  ( answer
  , possiblePasswords
  )
where

import           Data.List                      ( group )

hasTwoAdj :: Int -> Bool
hasTwoAdj = elem 2 . map length . group . show
-- Convert num to string, group the string by matching digits, 
-- get the length of each sublist, ensure AT least one sublist has a length of two.

notDecreasing :: Int -> Bool
notDecreasing num = all (increasing) digits
 where
  num' = show num
  increasing (a, b) = a <= b
  digits = zip num' (tail num')

possiblePasswords :: Int -> Int -> [Int]
possiblePasswords min max =
  [ x | x <- [min .. max], hasTwoAdj x && notDecreasing x ]

answer :: IO ()
answer = do
  putStrLn
    $  "Day 4, Part 2 Answer: "
    ++ (show $ length $ possiblePasswords 138307 654504)
