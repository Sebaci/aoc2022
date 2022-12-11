module Main where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2), ZipList (ZipList, getZipList))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (mapAccumL, mapAccumR)
import System.Environment (getArgs)

type Grid a = ZipList (ZipList a)

type Heightmap = Grid Int

type VisibilityMap = Grid (Maybe Int)

type Treerow = ZipList Int

type VisibilityRow = ZipList (Maybe Int)

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parse input

solve :: Heightmap -> Int
solve = countVisibleTrees . makeVisibilityMap

countVisibleTrees :: VisibilityMap -> Int
countVisibleTrees = length . foldMap (catMaybes . getZipList)

makeVisibilityMap :: Heightmap -> VisibilityMap
makeVisibilityMap hmap = liftA2 (<|>) <$> visibleByRows <*> visibleByColumns
  where
    visibleByRows = fmap markVisible hmap
    visibleByColumns = transpose $ markVisible <$> transpose hmap

markVisible :: Treerow -> VisibilityRow
markVisible treerow = (<|>) <$> fromLeft <*> fromRight
  where
    fromLeft = snd $ mapAccumL fAccum (-1) treerow
    fromRight = snd $ mapAccumR fAccum (-1) treerow

    fAccum curMax tree = (max curMax tree, maybeTaller curMax tree)
    maybeTaller height treeHeight
      | treeHeight > height = Just treeHeight
      | otherwise = Nothing

transpose :: Grid a -> Grid a
transpose = sequenceA

parse :: T.Text -> Heightmap
parse text = ZipList $ ZipList <$> (map $ map (read . (: [])) . T.unpack) (T.lines text)
