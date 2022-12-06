module Main where

import Data.Foldable (toList)
import Data.List (findIndex, nub)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parse input

solve :: [String] -> Int
solve = sum . map (countCharsToprocess 14)

countCharsToprocess :: Int -> String -> Int
countCharsToprocess markerLength line = case findIndex ((== markerLength) . length . nub . toList) markerCandidates of
  Just n -> n + markerLength
  _ -> error "Marker not found!"
  where
    markerCandidates = scanl moveWindow (Seq.fromList $ take markerLength line) (drop markerLength line)
    moveWindow window char = Seq.drop 1 window |> char

parse :: T.Text -> [String]
parse = map T.unpack . T.lines
