module Main where

import Control.Monad (MonadPlus (mplus), guard, join)
import Data.List (mapAccumL)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Coord = (Int, Int)

type Cave = Set Coord

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parseInput input

solve :: Cave -> Int
solve cave =
  let dropFrom = (500, 0)
      sandCoords = snd $ mapAccumL (const . dropSand dropFrom) cave [1 ..]
   in length $ takeWhile isJust sandCoords

dropSand :: Coord -> Cave -> (Cave, Maybe Coord)
dropSand from cave =
  let targetCoord = findTargetCoord (fromJust $ floorFrom from cave) cave
   in maybe (cave, Nothing) updateCave targetCoord
  where
    updateCave c = (Set.insert c cave, Just c)

findTargetCoord :: Coord -> Cave -> Maybe Coord
findTargetCoord (x, y) cave =
  let leftFloor = floorFrom (x - 1, y) cave
      rightFloor = floorFrom (x + 1, y) cave
   in join $ do
        l <- leftFloor
        continueIfLower l `mplus` do
          r <- rightFloor
          continueIfLower r `mplus` return (return (x, y - 1))
  where
    continueIfLower c@(_, y') = guard (y' > y) >> return (findTargetCoord c cave)

floorFrom :: Coord -> Cave -> Maybe Coord
floorFrom coord@(x, _) cave = Set.lookupGE coord cave >>= checkSameHorizontal
  where
    checkSameHorizontal c@(x', _) = c <$ guard (x == x')

parseInput :: T.Text -> Cave
parseInput input =
  let paths = map parsePath . T.lines $ input
      allLines = concatMap pathToLines paths
      rocks = concatMap lineToRocks allLines
   in Set.fromList rocks
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
