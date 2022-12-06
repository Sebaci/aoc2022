module Main where

import Data.Array
import Data.Char (isLetter)
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Stack = String

type Instruction = (Int, Int, Int)

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  let (stack, instructions) = parse input
  print $ solve stack instructions

solve :: Array Int Stack -> [Instruction] -> [Char]
solve = (map head .) . (elems .) . foldl performInstruction

performInstruction :: Array Int Stack -> Instruction -> Array Int Stack
performInstruction stack (amount, iFrom, iTo) = stack // [(iFrom, left), (iTo, right)]
  where
    (left, right) = move amount (stack ! iFrom) (stack ! iTo)

    move 0 s1 s2 = (s1, s2)
    move n (s : s1) s2 = move (n -1) s1 (s : s2)
    move _ s1 s2 = (s1, s2)

parse :: T.Text -> (Array Int Stack, [Instruction])
parse text = case break (== T.empty) (T.lines text) of
  (stackPart, empty : instructionPart) -> (parseStack stackPart, parseInstructions instructionPart)
  _ -> inputError
  where
    parseStack tStack = listArray (1, length stackList) stackList
      where
        stackList = parseToStackList tStack
        parseToStackList = map (filter isLetter) . transpose . map (map (\c -> T.unpack c !! 1) . T.chunksOf 4)
    parseInstructions tInstructions = parseToInstrList tInstructions
      where
        parseToInstrList = map parseInstruction
        parseInstruction i = let iw = (words $ T.unpack i) in (read $ iw !! 1, read $ iw !! 3, read $ iw !! 5)

inputError :: a
inputError = error "Bad input"
