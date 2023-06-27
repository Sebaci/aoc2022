{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.List (find, sort)
import Data.Maybe (fromJust, mapMaybe)
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
  let distressMin = 0
      distressMax = 4_000_000
      multiplier = 4_000_000

      rowsConsidered = [distressMin .. distressMax]
      rowSearchRange = (distressMin, distressMax)

      coverage = map (rowCoverage sensors) rowsConsidered
      (distressX, distressY) = fromJust . asum . map (findDistressBeacon rowSearchRange) $ coverage
   in distressX * multiplier + distressY

findDistressBeacon :: Range -> (Row, [Range]) -> Maybe Coord
findDistressBeacon (from, to) (row, coverage) = case dropWhile ((< from) . snd) coverage of
  [] -> Nothing
  (x1, x2) : _ -> case (x1 > from, x2 < to) of
    (True, _) -> Just (from, row)
    (False, True) -> Just (x2 + 1, row)
    _ -> Nothing

rowCoverage :: [Sensor] -> Row -> (Row, [Range])
rowCoverage sensors n =
  let sensorCoverages = mapMaybe (sensorCoverageAt n) sensors
      coverage = foldl addRangeToCoverage [] sensorCoverages
   in (n, coverage)

sensorCoverageAt :: Row -> Sensor -> Maybe Range
sensorCoverageAt n s@Sensor {x, y} =
  let distance = sensorDistance s
      yDifference = abs (y - n)
      xDistance = (distance - yDifference)
   in if xDistance < 0
        then Nothing
        else Just (x - xDistance, x + xDistance)

sensorDistance :: Sensor -> Int
sensorDistance Sensor {x, y, beacon = (bx, by)} = abs (bx - x) + abs (by - y)

addRangeToCoverage :: [Range] -> Range -> [Range]
addRangeToCoverage [] r = [r]
addRangeToCoverage (c : cs) r
  | rangeMergePossible r c = addRangeToCoverage cs (mergeRanges c r)
  | r < c = r : c : cs
  | otherwise = c : addRangeToCoverage cs r

rangeMergePossible :: Range -> Range -> Bool
rangeMergePossible (s1, e1) (s2, e2) = not $ e1 < s2 - 1 || e2 < s1 - 1

mergeRanges :: Range -> Range -> Range
mergeRanges (s1, e1) (s2, e2) = (min s1 s2, max e1 e2)

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
