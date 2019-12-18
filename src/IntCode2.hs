-- |
-- Module      :  IntCode2
-- Description :  Day 5 - Improve previous Intcode machine
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

{-# LANGUAGE BangPatterns #-}

module IntCode2
  ( answer
  )
where

import           Data.Maybe
import           System.IO
import Control.Exception ( try )
import Data.Either ( either )
import           Data.List.Split
import qualified Data.Map.Strict               as M

type InstructionPointer = Int

type Tape = [Int]
type Address = Int
type Intcode = M.Map Address Int

data OpCode = Halt | Add | Multiply | Input | Output |JIT | JIF | Less | Equals
    deriving ( Show, Eq, Ord )

data ParameterMode = Immediate | Position
    deriving ( Show, Eq, Ord )

type Instruction = (OpCode, [Int])

opcode :: Int -> OpCode
opcode n = case n of
  99 -> Halt
  1  -> Add
  2  -> Multiply
  3  -> Input
  4  -> Output
  5  -> JIT
  6  -> JIF
  7  -> Less
  8  -> Equals
  _  -> Halt

parameterMode :: Int -> ParameterMode
parameterMode n = case n of
  0 -> Position
  1 -> Immediate

readIP :: InstructionPointer -> Intcode -> Maybe Instruction
readIP ip program = do
  op <- M.lookup ip program
  let (opCode, params) = evalDigits $ pad 5 $ digits op in 
    Just (opCode, readParams params)
 where
  readParams modes =
    map (getParameter program) $ zip modes [(ip + 1) .. (ip + length modes)]

getParameter :: Intcode -> (ParameterMode, Address) -> Int
getParameter tape (mode, address) = case getParameter' mode address of
  Just n -> n
  Nothing -> 0
 where 
  getParameter' Immediate address = M.lookup address tape
  getParameter' Position address = M.lookup address tape >>= (\x -> M.lookup x tape)

pad :: Int -> [Int] -> [Int]
pad n lst | len > n   = take n lst
          | len < n   = lst ++ take (n - len) (repeat 0)
          | otherwise = lst
  where len = length lst

evalDigits :: [Int] -> (OpCode, [ParameterMode])
evalDigits (ones : tens : rest) =
  case opcode $ ones + (tens * 10) of 
    Halt     -> (Halt, [])
    Add      -> (Add, evalDigits' $ init rest)
    Multiply -> (Multiply, evalDigits' $ init rest)
    Input    -> (Input,  [Immediate])
    Output   -> (Output, [Immediate])
    JIT      -> (JIT, init $ evalDigits' $ take 2 rest)
    JIF      -> (JIF, init $ evalDigits' $ take 2 rest)
    Less     -> (Less, evalDigits' $ init rest)
    Equals   -> (Equals, evalDigits' $ init rest)
 where
  evalDigits' []       = Immediate : []
  evalDigits' (n : ns) = parameterMode n : evalDigits' ns

digits :: Integral x => x -> [x]
digits x | x > 0     = x `mod` 10 : digits (x `div` 10)
         | otherwise = []

-- Input comes in as a list of ints
readTape :: Tape -> Intcode
readTape intcode = M.fromList $ zip [0 ..] intcode

run :: InstructionPointer -> Intcode -> IO Tape
run ip program = eval $ readIP ip program
 where
  execute' = execute program
  eval (Just (Halt, _)) = return $ halt program
  eval Nothing          = return $ halt program
  eval (Just (opcode, args))  = do
    (pointer, program) <- execute' (opcode, args) ip
    run pointer program

execute :: Intcode -> Instruction -> InstructionPointer -> IO (InstructionPointer, Intcode)
execute program (Halt, _) ip = pure $ (ip, program)

execute program (Add, args) ip =
  pure $ (ip + length args + 1, M.insert (last args) (foldr (+) 0 $ init args) program)

execute program (Multiply, args) ip =
  pure $ (ip + length args + 1, M.insert (last args) (foldr (*) 1 $ init args) program)

execute program (Input, [address]) ip = do 
  putStr "> " >> hFlush stdout
  val <- getLine
  pure $ (ip + 2, M.insert address (read val :: Int) program)

execute program (Output, [address]) ip = do 
  case M.lookup address program of
    Just n  -> putStrLn $ show n
    Nothing -> (putStrLn $ "No value at address: " ++ (show address))
  pure $ (ip + 2, program)

execute program (JIT, (a:b:rest)) ip = if a > 0 then pure $ (b, program) else pure $ (ip + 3, program)
execute program (JIF, (a:b:rest)) ip = if a == 0 then pure $ (b, program) else pure $ (ip + 3, program)

execute program (Less, (a:b:address:rest)) ip = case a < b of
  True  -> pure $ (ip + 4, M.insert address 1 program)
  False -> pure $ (ip + 4, M.insert address 0 program)

execute program (Equals, (a:b:address:rest)) ip = case a == b of
  True  -> pure $ (ip + 4, M.insert address 1 program)
  False -> pure $ (ip + 4, M.insert address 0 program)

halt :: Intcode -> Tape
halt program = M.elems program

answer :: IO ()
answer = do
  d5_data <- parseData "data/d5_input.txt" :: IO [Int]
  run 0 $ readTape d5_data
  putStrLn "Day 5, Part 1 Finished"
  run 0 $ readTape d5_data
  putStrLn "Day 5, Part 2 Finished"
 where
  parseData file =
    readFile file >>= return <$> Prelude.map (read) . (splitOn ",")
