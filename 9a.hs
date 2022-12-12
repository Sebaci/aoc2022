module Main where

import Data.Bifunctor (Bifunctor (first, second))
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Position = (Int, Int)

type Move = Position -> Position

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parse input

solve :: [Move] -> Int
solve moves = length . nub . map snd $ motionHistory
  where
    motionHistory = simulateMotions ((0, 0), (0, 0)) moves

simulateMotions :: (Position, Position) -> [Move] -> [(Position, Position)]
simulateMotions = scanl step
  where
    step (head, tail) move = (head', tail')
      where
        tail' = moveTail head head' tail
        head' = move head

moveTail :: Position -> Position -> Position -> Position
moveTail (headX, headY) (headX', headY') (tailX, tailY)
  | abs (headX' - tailX) <= 1 && abs (headY' - tailY) <= 1 = (tailX, tailY)
  | headX' == tailX || headY' == tailY = ((headX' + tailX) `div` 2, (headY' + tailY) `div` 2)
  | otherwise = (headX, headY)

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
