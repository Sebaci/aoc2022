module Main where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2), ZipList (ZipList, getZipList))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (mapAccumL, mapAccumR)
import System.Environment (getArgs)

type Grid a = ZipList (ZipList a)

type Heightmap = Grid Int

type ScenicScoreMap = Grid Int

type Treerow = ZipList Int

type RowDistances = ZipList Int

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parse input

solve :: Heightmap -> Int
solve = maximum . foldMap getZipList . toScenicScoreMap

toScenicScoreMap :: Heightmap -> ScenicScoreMap
toScenicScoreMap hmap = liftA2 (*) <$> distanceByRows <*> distanceByColumns
  where
    distanceByRows = fmap rowDistanceScore hmap
    distanceByColumns = transpose $ rowDistanceScore <$> transpose hmap

rowDistanceScore :: Treerow -> RowDistances
rowDistanceScore list = (*) <$> fromLeft <*> fromRight
  where
    fromLeft = snd $ mapAccumL fAccum [] list
    fromRight = snd $ mapAccumR fAccum [] list

    fAccum [] currentTree = ([currentTree], 0)
    fAccum treesSoFar currentTree = (currentTree : treesSoFar, viewingDistance)
      where
        viewingDistance = case span (< currentTree) treesSoFar of
          (shorterTrees, []) -> length shorterTrees
          (shorterTrees, ts) -> length shorterTrees + 1

transpose :: Grid a -> Grid a
transpose = sequenceA

parse :: T.Text -> Heightmap
parse text = ZipList $ ZipList <$> (map $ map (read . (: [])) . T.unpack) (T.lines text)
