module Main where

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Assignment = (Int, Int)

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ length $ filter (uncurry overlap) (parse input)

overlap :: Assignment -> Assignment -> Bool
overlap (startA, endA) (startB, endB) = not (endA < startB || endB < startA)

parse :: T.Text -> [(Assignment, Assignment)]
parse = map parseLine . T.lines
  where
    parseLine line = case map (read . T.unpack) $ T.split (\c -> c == '-' || c == ',') line of
      [startA, endA, startB, endB] -> ((startA, endA), (startB, endB))
      _ -> error "Bad input"
