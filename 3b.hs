module Main where

import Data.Char (ord)
import Data.List (intersect, unfoldr)
import System.Environment (getArgs)

type Group = (Rucksack, Rucksack, Rucksack)

type Rucksack = [Item]

type Item = Char

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print (solve $ parse input)

solve :: [Group] -> Int
solve = sum . map (priority . findCommonItem)

findCommonItem :: Group -> Item
findCommonItem (r1, r2, r3) = head $ r1 `intersect` r2 `intersect` r3

priority :: Char -> Int
priority char = if char > 'Z' then ord char - 96 else ord char - 38

parse :: String -> [Group]
parse = groupRucksacks . lines

groupRucksacks :: [Rucksack] -> [Group]
groupRucksacks = map toGroup . takeWhile (not . null) . unfoldr (Just . splitAt 3)
  where
    toGroup [r1, r2, r3] = (r1, r2, r3)
    toGroup _ = error "Bad input"
