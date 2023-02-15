{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.State (MonadState (get), State, evalState, modify, when)
import Data.Array (Array, assocs, bounds, listArray, (!))
import Data.Char (ord)
import Data.List (find)
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set, insert, member)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Position = (Int, Int)

type Steps = Int

type Heightmap = Array Position Char

data SearchState = SearchState
  { visited :: Set Position,
    queue :: Seq (Position, Steps)
  }

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve (parse input)

solve :: Heightmap -> Int
solve hmap = evalState (trip hmap) initState
  where
    initState = SearchState (Set.singleton startPosition) (Seq.singleton (startPosition, 0))
    startPosition = findStartPosition hmap

trip :: Heightmap -> State SearchState Int
trip hmap = do
  st@SearchState {visited, queue} <- get
  case queue of
    Empty -> error "destination unreachable!"
    current@(pos, steps) :<| qs ->
      case hmap ! pos of
        'E' -> return steps
        _ ->
          modify removeFirstFromQueue
            >> mapM_ (checkMove hmap current) (neighbors pos hmap)
            >> trip hmap
      where
        removeFirstFromQueue st = st {queue = qs}

checkMove :: Heightmap -> (Position, Steps) -> Position -> State SearchState ()
checkMove hmap (from, steps) to = do
  st@SearchState {visited} <- get
  let alreadyVisited = to `member` visited
      isReachable = canMakeMove hmap from to

  when (not alreadyVisited && isReachable) $ modify addPositionToQueue
  where
    addPositionToQueue st@SearchState {visited, queue} =
      st {visited = insert to visited, queue = queue |> (to, steps + 1)}

canMakeMove :: Heightmap -> Position -> Position -> Bool
canMakeMove hmap fromPos toPos = ord to' - ord from' <= 1
  where
    from'
      | from == 'S' = 'a'
      | otherwise = from
    to'
      | to == 'E' = 'z'
      | otherwise = to
    from = hmap ! fromPos
    to = hmap ! toPos

findStartPosition :: Heightmap -> Position
findStartPosition hmap = case find ((== 'S') . snd) (assocs hmap) of
  Nothing -> error "No starting point!"
  Just (pos, _) -> pos

neighbors :: Position -> Heightmap -> [Position]
neighbors (y, x) hmap = filter withinBounds possibleNeighbors
  where
    withinBounds (y, x) = y >= minY && y <= maxY && x >= minX && x <= maxX
    possibleNeighbors = [(y + 1, x), (y, x + 1), (y - 1, x), (y, x - 1)]
    ((minY, minX), (maxY, maxX)) = bounds hmap

parse :: T.Text -> Heightmap
parse text = listArray ((1, 1), (height, width)) $ concat rows
  where
    height = length rows
    width = length $ head rows
    rows = map T.unpack $ T.lines text
