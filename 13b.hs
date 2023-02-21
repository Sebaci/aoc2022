module Main where

import Data.List (elemIndex, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

data Token = Lbr | Rbr | Comma | IntVal Int deriving (Show)

data Item = Arr NestedArray | Val Int deriving (Eq, Show)

type NestedArray = [Item]

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parseInput input

solve :: [NestedArray] -> Int
solve packetPairs =
  let divider1 = [Arr [Val 2]]
      divider2 = [Arr [Val 6]]
      orderedPackets = sortBy comparePackets (divider1 : divider2 : packetPairs)
      (Just divIndex1) = elemIndex divider1 orderedPackets
      (Just divIndex2) = elemIndex divider2 orderedPackets
   in (divIndex1 + 1) * (divIndex2 + 1)

tokenize :: String -> [Token]
tokenize = go ""
  where
    go n [] = [toIntVal n | n /= ""]
    go n (c : xs)
      | isDigit c = go (c : n) xs
      | n == "" = charToToken c : go "" xs
      | otherwise = toIntVal n : charToToken c : go "" xs

    toIntVal = IntVal . read . reverse

    isDigit c = c `elem` ['0' .. '9']

    charToToken '[' = Lbr
    charToToken ']' = Rbr
    charToToken ',' = Comma
    charToToken _ = error "unrecognized token"

parse :: [Token] -> NestedArray
parse tokens =
  let (Arr arr, _) = go [] tokens
   in arr
  where
    go [arr] [] = (arr, [])
    go acc (IntVal v : ts) = go (Val v : acc) ts
    go acc (Comma : ts) = go acc ts
    go acc (Rbr : ts) = (Arr $ reverse acc, ts)
    go acc (Lbr : ts) = go (arr : acc) ts2
      where
        (arr, ts2) = go [] ts
    go _ _ = error "Parse error"

comparePackets :: NestedArray -> NestedArray -> Ordering
comparePackets [] [] = EQ
comparePackets _ [] = GT
comparePackets [] _ = LT
comparePackets (Val v1 : ps1) (Val v2 : ps2) = compare v1 v2 <> comparePackets ps1 ps2
comparePackets (Arr a1 : ps1) (Arr a2 : ps2) = comparePackets a1 a2 <> comparePackets ps1 ps2
comparePackets (Val v : ps1) ps2 = comparePackets (Arr [Val v] : ps1) ps2
comparePackets ps1 (Val v : ps2) = comparePackets ps1 (Arr [Val v] : ps2)

parseInput :: T.Text -> [NestedArray]
parseInput input =
  let rawPackets = filter ((> 0) . T.length) $ T.lines input
   in map (parse . tokenize . T.unpack) rawPackets
