module Main where

import Data.List (sort)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  res <- solve . parseInput <$> readFile (head args)
  print res

solve :: [[Int]] -> Int
solve = sum . take 3 . reverse . sort . map sum

parseInput :: String -> [[Int]]
parseInput input = foldr go [[]] (lines input)
  where
    go "" elves = [] : elves
    go calorie (e : elves) = (read calorie : e) : elves
    go _ _ = error "Bad input"
