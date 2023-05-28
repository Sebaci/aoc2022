{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Char (isDigit)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Coord = (Int, Int)

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
  let n = 2_000_000
      coverage = Set.unions $ map (coverageAt n) sensors
      takenCoords = Set.unions $ map (takenCoordsAt n) sensors
   in Set.size (coverage \\ takenCoords)

coverageAt :: Int -> Sensor -> Set Coord
coverageAt n s@Sensor {x, y} =
  let distance = sensorDistance s
      yDifference = abs (y - n)
   in Set.fromAscList [(x, n) | x <- [x - (distance - yDifference) .. x + (distance - yDifference)]]

takenCoordsAt :: Int -> Sensor -> Set Coord
takenCoordsAt n s@Sensor {x, y, beacon} =
  Set.fromList $ filter ((== n) . snd) [(x, y), beacon]

sensorDistance :: Sensor -> Int
sensorDistance Sensor {x, y, beacon = (bx, by)} = abs (bx - x) + abs (by - y)

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
