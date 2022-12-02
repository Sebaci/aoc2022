module Main where

import Data.List
import System.Environment (getArgs)

data RPS = Rock | Paper | Scissors

type Round = (RPS, RPS)

type Score = Int

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print (solve $ parse input)

solve :: [Round] -> Score
solve = sum . map roundScore

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

rps :: String -> RPS
rps symbol = case symbol of
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors
  "X" -> Rock
  "Y" -> Paper
  "Z" -> Scissors
  _ -> error "Bad input"

parse :: String -> [Round]
parse = map (toRoundPair . map rps . words) . lines
  where
    toRoundPair [p1, p2] = (p1, p2)
    toRoundPair _ = error "Bad input"
