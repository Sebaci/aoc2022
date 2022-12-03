module Main where

import Data.List
import System.Environment (getArgs)

data RPS = Rock | Paper | Scissors

data Result = Win | Lose | Draw

type RoundGuide = (RPS, Result)

type Round = (RPS, RPS)

type Score = Int

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print (solve $ parse input)

solve :: [RoundGuide] -> Score
solve = sum . map (roundScore . inferRound)

roundScore :: Round -> Score
roundScore round = winScore round + shapeScore (snd round)

shapeScore :: RPS -> Score
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

winScore :: Round -> Score
winScore (Paper, Rock) = 0
winScore (Scissors, Rock) = 6
winScore (Rock, Paper) = 6
winScore (Scissors, Paper) = 0
winScore (Rock, Scissors) = 0
winScore (Paper, Scissors) = 6
winScore (_, _) = 3

inferRound :: RoundGuide -> Round
inferRound (sym1, result) = (sym1, sym2)
  where
    sym2 = case (sym1, result) of
      (Rock, Win) -> Paper
      (Rock, Lose) -> Scissors
      (Paper, Win) -> Scissors
      (Paper, Lose) -> Rock
      (Scissors, Win) -> Rock
      (Scissors, Lose) -> Paper
      _ -> sym1

rps :: String -> RPS
rps symbol = case symbol of
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors
  _ -> badInputError

result :: String -> Result
result symbol = case symbol of
  "X" -> Lose
  "Y" -> Draw
  "Z" -> Win
  _ -> badInputError

parse :: String -> [RoundGuide]
parse = map (toRound . words) . lines
  where
    toRound [s1, s2] = (rps s1, result s2)
    toRound _ = badInputError

badInputError :: a
badInputError = error "Bad input"