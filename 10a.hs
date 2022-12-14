module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

data Instruction = Noop | Addx Int

data EvalState = EvalState {regX :: Int, cycleCount :: Int}

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parse input

solve :: [Instruction] -> Int
solve instructions = sum . take 6 . map signalStrength $ (20 : [60, 100 ..])
  where
    signalStrength cycle = cycle * regX (head (dropWhile ((< (cycle -2)) . cycleCount) evalStates))

    evalStates = evaluate instructions

evaluate :: [Instruction] -> [EvalState]
evaluate = scanl modify (EvalState 1 0)
  where
    modify (EvalState regX cycleCount) Noop = EvalState regX (cycleCount + 1)
    modify (EvalState regX cycleCount) (Addx n) = EvalState (regX + n) (cycleCount + 2)

parse :: T.Text -> [Instruction]
parse = map (toInstruction . T.unpack) . T.lines
  where
    toInstruction "noop" = Noop
    toInstruction instr = case words instr of
      ["addx", v] -> Addx $ read v
      _ -> error "Unknown instruction!"