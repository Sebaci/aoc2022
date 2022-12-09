module Main where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tree (Tree (Node, rootLabel), foldTree)
import System.Environment (getArgs)

data Item
  = Directory {name :: String, size :: Int}
  | File {name :: String, size :: Int}
  deriving (Show)

type FS = Tree Item

data TerminalHistory = Cd String | CdUp | Ls | Dir String | FileInfo Int String deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  let dirTree = buildTree $ parse $ T.lines input
  print $ solve 70000000 30000000 dirTree

solve :: Int -> Int -> FS -> Int
solve totalSpace requiredSpace fs = foldl selectOptimum usedSpace fs
  where
    selectOptimum curSize (File _ _) = curSize
    selectOptimum curSize (Directory _ dirSize)
      | dirSize < curSize && dirSize >= spaceToFree = dirSize
      | otherwise = curSize

    spaceToFree = requiredSpace - (totalSpace - usedSpace)
    usedSpace = size $ rootLabel fs

buildTree :: [TerminalHistory] -> FS
buildTree hs = head $ snd $ go hs []
  where
    getSize = sum . map (size . rootLabel)
    go (Cd dir : hs) fs = go hsRest $ Node (Directory dir (getSize children)) children : fs
      where
        (hsRest, children) = go hs []
    go (CdUp : hs) fs = (hs, fs)
    go (Ls : hs) fs = go hs []
    go (Dir name : hs) fs = go hs fs
    go (FileInfo size name : hs) fs = go hs (Node (File name size) [] : fs)
    go [] fs = ([], fs)

parse :: [T.Text] -> [TerminalHistory]
parse = map (parseLine . T.unpack)
  where
    parseLine line
      | line == "$ cd .." = CdUp
      | "$ cd" `isPrefixOf` line = Cd $ words line !! 2
      | line == "$ ls" = Ls
      | "dir" `isPrefixOf` line = Dir (words line !! 1)
      | otherwise = FileInfo <$> read . head <*> last $ words line
