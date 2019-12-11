-- |
-- Module      :  IntCode
-- Description :  Day 2 - Create a machine to run an Intcode program
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module IntCode
  ( readIntcode
  , run
  , d2_data
  )
where

import           Data.Map.Strict               as M
import           Data.Maybe

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

d2_data :: [Int]
d2_data =
  [ 1
  , 0
  , 0
  , 3
  , 1
  , 1
  , 2
  , 3
  , 1
  , 3
  , 4
  , 3
  , 1
  , 5
  , 0
  , 3
  , 2
  , 1
  , 9
  , 19
  , 1
  , 10
  , 19
  , 23
  , 2
  , 9
  , 23
  , 27
  , 1
  , 6
  , 27
  , 31
  , 2
  , 31
  , 9
  , 35
  , 1
  , 5
  , 35
  , 39
  , 1
  , 10
  , 39
  , 43
  , 1
  , 10
  , 43
  , 47
  , 2
  , 13
  , 47
  , 51
  , 1
  , 10
  , 51
  , 55
  , 2
  , 55
  , 10
  , 59
  , 1
  , 9
  , 59
  , 63
  , 2
  , 6
  , 63
  , 67
  , 1
  , 5
  , 67
  , 71
  , 1
  , 71
  , 5
  , 75
  , 1
  , 5
  , 75
  , 79
  , 2
  , 79
  , 13
  , 83
  , 1
  , 83
  , 5
  , 87
  , 2
  , 6
  , 87
  , 91
  , 1
  , 5
  , 91
  , 95
  , 1
  , 95
  , 9
  , 99
  , 1
  , 99
  , 6
  , 103
  , 1
  , 103
  , 13
  , 107
  , 1
  , 107
  , 5
  , 111
  , 2
  , 111
  , 13
  , 115
  , 1
  , 115
  , 6
  , 119
  , 1
  , 6
  , 119
  , 123
  , 2
  , 123
  , 13
  , 127
  , 1
  , 10
  , 127
  , 131
  , 1
  , 131
  , 2
  , 135
  , 1
  , 135
  , 5
  , 0
  , 99
  , 2
  , 14
  , 0
  , 0
  ]

