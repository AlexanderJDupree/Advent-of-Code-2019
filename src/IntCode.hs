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
