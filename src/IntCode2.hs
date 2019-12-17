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

data OpCode = Halt | Add | Multiply | Input | Output
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
  _  -> Halt

parameterMode :: Int -> ParameterMode
parameterMode n = case n of
  0 -> Position
  1 -> Immediate

incrementIP :: InstructionPointer -> InstructionPointer
incrementIP ip = ip + 4

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
    result <- execute' (opcode, args)
    run (ip + length args + 1) result

execute :: Intcode -> Instruction -> IO Intcode
execute program (Halt, _) = pure $ program

execute program (Add, args) =
  pure $ M.insert (last args) (foldr (+) 0 $ init args) program

execute program (Multiply, args) =
  pure $ M.insert (last args) (foldr (*) 1 $ init args) program

execute program (Input, [address]) = do 
  val <- getLine
  pure $ M.insert address (read val :: Int) program

execute program (Output, [address]) = do 
  case M.lookup address program of
    Just n  -> putStrLn $ show n
    Nothing -> (putStrLn $ "No value at address: " ++ (show address))
  pure $ program

halt :: Intcode -> Tape
halt program = M.elems program

answer :: IO ()
answer = do
  d5_data <- parseData "data/d5_input.txt" :: IO [Int]
  run 0 $ readTape d5_data
  putStrLn "Day 5, Part 1 Finished"
 where
  parseData file =
    readFile file >>= return <$> Prelude.map (read) . (splitOn ",")
