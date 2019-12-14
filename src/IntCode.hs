-- |
-- Module      :  IntCode
-- Description :  Day 2 - Create a machine to run an Intcode program
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module IntCode
  ( readIntcode
  , answer
  , run
  )
where

import           Data.Map.Strict               as M
import           Data.Maybe
import           System.IO
import           Data.List.Split

type InstructionPointer = Int

type Intcode = [Int]
type Address = Int
type Program = M.Map Address Int

data OpCode = Halt | Add | Multiply
    deriving ( Show, Eq, Ord )

type Instruction = (OpCode, Int, Int, Address)

opcode n = case n of
  99 -> Halt
  1  -> Add
  2  -> Multiply
  _  -> Halt

incrementIP :: InstructionPointer -> InstructionPointer
incrementIP ip = ip + 4

readIP :: InstructionPointer -> Program -> Maybe Instruction
readIP ip program = do
  op      <- M.lookup ip program
  a       <- readAddress $ ip + 1
  b       <- readAddress $ ip + 2
  address <- M.lookup (ip + 3) program
  pure ((opcode op), a, b, address)
 where
  readAddress address = M.lookup address program >>= (\x -> M.lookup x program)

-- Input comes in as a list of ints
readIntcode :: Intcode -> Program
readIntcode intcode = M.fromList $ zip [0 ..] intcode

run :: InstructionPointer -> Program -> Intcode
run ip program = eval $ readIP ip program
 where
  eval (Just (Halt, _, _, _)) = halt program
  eval Nothing                = halt program
  eval (Just instruction)     = run (incrementIP ip) $ execute' instruction
  execute' = execute program

execute :: Program -> Instruction -> Program
execute program (Halt    , a, b, address) = program
execute program (Add     , a, b, address) = M.insert address (a + b) program
execute program (Multiply, a, b, address) = M.insert address (a * b) program

halt :: Program -> Intcode
halt program = M.elems program

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
  prepareData (n, v) =
    readIntcode $ head d2_data : [n, v] ++ (Prelude.drop 3 d2_data)

answer :: IO ()
answer = do
  d2_data <- parseData "data/d2_input.txt"
  putStrLn
    $  "Day 2, Part 1 Answer: "
    ++ (show $ head $ run 0 $ readIntcode d2_data)
  putStrLn $ "Day 2, Part 2 Answer: " ++ (show $ computePair 19690720 d2_data)
 where
  parseData file =
    readFile file >>= return <$> Prelude.map (read) . (splitOn ",")
