module Main where

import Data.List (unfoldr)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

data Token = Lbr | Rbr | Comma | IntVal Int deriving (Show)

data Item = Arr NestedArray | Val Int deriving (Show)

type NestedArray = [Item]

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve $ parseInput input

solve :: [(NestedArray, NestedArray)] -> Int
solve packetPairs =
  let comparisons = map (uncurry comparePackets) packetPairs
   in sum . map fst . filter ((/= GT) . snd) . zip [1 ..] $ comparisons

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

parseInput :: T.Text -> [(NestedArray, NestedArray)]
parseInput input =
  let triples = takeWhile (not . null) $ unfoldr (Just . splitAt 3) (T.lines input)
   in map toPacketPair triples
  where
    toPacketPair (first : second : _) = (toPacket first, toPacket second)
    toPacketPair x = error $ "bad input" ++ show x
    toPacket = parse . tokenize . T.unpack
