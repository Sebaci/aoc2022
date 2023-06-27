{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Coord = (Int, Int)

data Sensor = Sensor
  { x :: Int,
    y :: Int,
    beacon :: Coord
  }
  deriving (Eq, Show)

type BCoefficient = Int

data Line = Increasing BCoefficient | Decreasing BCoefficient deriving (Eq)

type Bound = [Line]

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parseInput input

solve :: [Sensor] -> Int
solve sensors =
  let corners = [(x, y) | x <- [distressMin, distressMax], y <- [distressMin, distressMax]]
      intersectionCoords = boundIntersections sensors
      distressCoordCandidates = corners ++ filter inConsideredArea intersectionCoords
      (x, y) = fromJust . find (not . covered) $ distressCoordCandidates
   in x * multiplier + y
  where
    multiplier = 4_000_000
    distressMin = 0
    distressMax = 4_000_000
    inConsideredArea (x, y) = x >= distressMin && x <= distressMax && y >= distressMin && y <= distressMax
    covered c = any (covers c) sensors

covers :: Coord -> Sensor -> Bool
covers (cx, cy) sensor@Sensor {x, y} =
  let distance = sensorDistance sensor
      coordDistance = manhattanDistance (cx, cy) (x, y)
   in coordDistance <= distance

boundIntersections :: [Sensor] -> [Coord]
boundIntersections sensors = do
  s1 <- sensors
  s2 <- sensors
  guard $ s1 /= s2
  intersections (sensorBound s1) (sensorBound s2)

intersections :: Bound -> Bound -> [Coord]
intersections b1 b2 =
  mapMaybe (uncurry intersection) [(line1, line2) | line1 <- b1, line2 <- b2]

sensorBound :: Sensor -> Bound
sensorBound s@Sensor {x, y} =
  let bDist = sensorDistance s + 1
   in [ Increasing $ y - bDist - x,
        Decreasing $ y - bDist + x,
        Increasing $ y + bDist - x,
        Decreasing $ y - bDist + x
      ]

intersection :: Line -> Line -> Maybe Coord
intersection line1 line2 = case (line1, line2) of
  (Increasing b1, Decreasing b2) -> aux b1 b2
  (Decreasing b1, Increasing b2) -> aux b1 b2
  (_, _) -> Nothing
  where
    aux b1 b2
      | even bSum = Just ((- bDiff) `div` 2, bSum `div` 2)
      | otherwise = Nothing
      where
        bSum = b1 + b2
        bDiff = b1 - b2

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
