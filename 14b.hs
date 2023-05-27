module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (mapAccumL, maximumBy)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Coord = (Int, Int)

type Height = Int

type Cave = Set Coord

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parseInput input

solve :: (Cave, Height) -> Int
solve (cave, height) =
  let sandX = 500
      dropFrom = (sandX, 0)
      caveWithFloor = addFloorToCave cave height sandX
      sandCoords = snd $ mapAccumL (const . dropSand dropFrom) caveWithFloor [1 ..]
   in length $ takeWhile isJust sandCoords
  where
    addFloorToCave cave height center =
      let heightWithFloor = height + 2
          floor = Set.fromList [(x, heightWithFloor) | x <- [center - heightWithFloor .. center + heightWithFloor]]
       in Set.union cave floor

dropSand :: Coord -> Cave -> (Cave, Maybe Coord)
dropSand from cave =
  let floor = floorFrom from cave
      targetCoord = guard (floor /= from) $> findTargetCoord floor cave
   in maybe (cave, Nothing) updateCave targetCoord
  where
    updateCave c = (Set.insert c cave, Just c)

findTargetCoord :: Coord -> Cave -> Coord
findTargetCoord coord@(x, y) cave =
  let leftFloor = floorFrom (x - 1, y) cave
      rightFloor = floorFrom (x + 1, y) cave
   in fromJust $ continueIfLower leftFloor <|> continueIfLower rightFloor <|> pure (x, y - 1)
  where
    continueIfLower c@(_, y') = guard (y' > y) $> findTargetCoord c cave

floorFrom :: Coord -> Cave -> Coord
floorFrom coord@(x, _) cave = fromJust $ Set.lookupGE coord cave

parseInput :: T.Text -> (Cave, Int)
parseInput input =
  let paths = map parsePath . T.lines $ input
      allLines = concatMap pathToLines paths
      rocks = concatMap lineToRocks allLines
      caveHeight = snd $ maximumBy (compare `on` snd) rocks
   in (Set.fromList rocks, caveHeight)
  where
    pathToLines path = zip path (tail path)
    lineToRocks ((x, y), (x', y')) = [(x'', y'') | x'' <- xs, y'' <- ys]
      where
        xs = [min x x' .. max x x']
        ys = [min y y' .. max y y']
    parsePath line = map parseCoord $ T.splitOn (T.pack " -> ") line
    parseCoord coord = (textToInt fst, textToInt snd)
      where
        [fst, snd] = T.splitOn (T.pack ",") coord
    textToInt = read . T.unpack
