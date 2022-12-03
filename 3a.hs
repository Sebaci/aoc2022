module Main where

import Data.Char (ord)
import Data.List (intersect)
import System.Environment (getArgs)

type Rucksack = (Compartment, Compartment)

type Compartment = [Item]

type Item = Char

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  print (solve $ parse input)

solve :: [Rucksack] -> Int
solve = sum . map (priority . findCommonItem)

findCommonItem :: Rucksack -> Item
findCommonItem (first, second) = head $ intersect first second

priority :: Char -> Int
priority char = if char > 'Z' then ord char - 96 else ord char - 38

parse :: String -> [Rucksack]
parse = map splitToCompartments . lines
  where
    splitToCompartments contents = splitAt (length contents `div` 2) contents