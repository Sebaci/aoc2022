module Main where

import Data.Bifunctor (Bifunctor (first, second))
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Position = (Int, Int)

type Rope = [Position]

type Move = Position -> Position

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parse input

solve :: [Move] -> Int
solve moves = length . nub . map last $ motionHistory
  where
    motionHistory = simulateMotions (replicate 10 (0, 0)) moves

simulateMotions :: Rope -> [Move] -> [Rope]
simulateMotions = scanl moveRope

moveRope :: Rope -> Move -> Rope
moveRope rope move = scanl moveKnot (move $ head rope) (tail rope)

moveKnot :: Position -> Position -> Position
moveKnot (prevX, prevY) (knotX, knotY)
  | abs (prevX - knotX) <= 1 && abs (prevY - knotY) <= 1 = (knotX, knotY)
  | abs (prevX - knotX) == 2 && abs (prevY - knotY) == 2 = ((prevX + knotX) `div` 2, (prevY + knotY) `div` 2)
  | abs (prevX - knotX) == 2 = ((prevX + knotX) `div` 2, prevY)
  | otherwise = (prevX, (prevY + knotY) `div` 2)

parse :: T.Text -> [Move]
parse = concatMap parseLine . T.lines
  where
    parseLine line = replicate n move
      where
        (move, n) = case words $ T.unpack line of
          [tmove, tamount] -> (parseMove tmove, read tamount)
          _ -> error "Bad input"

        parseMove "U" = second succ
        parseMove "D" = second pred
        parseMove "L" = first pred
        parseMove "R" = first succ
        parseMove _ = error "Incorrect move type"
