{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Coord = (Int, Int)

type Row = Int

type Range = (Int, Int)

data Sensor = Sensor
  { x :: Int,
    y :: Int,
    beacon :: Coord
  }
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parseInput input

solve :: [Sensor] -> Int
solve sensors =
  let multiplier = 4_000_000

      sensorsCandidates = map (filter inConsideredArea . nearestNoncovered) sensors
      nonCovered = map (filter (not . covered)) sensorsCandidates
      (x, y) = head . fromJust . find (not . null) $ nonCovered
   in x * multiplier + y
  where
    distressMin = 0
    distressMax = 4_000_000
    inConsideredArea (x, y) = x >= distressMin && x <= distressMax && y >= distressMin && y <= distressMax
    covered c = any (covers c) sensors

nearestNoncovered :: Sensor -> [Coord]
nearestNoncovered s@Sensor {x, y} =
  let distance = sensorDistance s
      sideDivisions = zip [1 .. distance] [distance, distance -1 .. 1]
      cornerDivisions = [(- distance - 1, 0), (distance + 1, 0), (0, - distance - 1), (0, distance + 1)]

      sidesPossibilities = [id, first negate, second negate, bimap negate negate]
      allDivisions = cornerDivisions ++ (sidesPossibilities <*> sideDivisions)
   in bimap (+ x) (+ y) <$> allDivisions

covers :: Coord -> Sensor -> Bool
covers (cx, cy) sensor@Sensor {x, y} =
  let distance = sensorDistance sensor
      coordDistance = manhattanDistance (cx, cy) (x, y)
   in coordDistance <= distance

sensorDistance :: Sensor -> Int
sensorDistance Sensor {x, y, beacon = (bx, by)} = manhattanDistance (x, y) (bx, by)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseInput :: T.Text -> [Sensor]
parseInput = map parseLine . T.lines

parseLine :: T.Text -> Sensor
parseLine line =
  let chunks = T.splitOn (T.pack " ") line
      digitChunks = map toInt . filter (not . null) . map (filter minusOrDigit . T.unpack) $ chunks
   in case digitChunks of
        [x, y, bx, by] -> Sensor x y (bx, by)
        _ -> error "bad input"
  where
    toInt = read :: String -> Int
    minusOrDigit c = isDigit c || c == '-'
