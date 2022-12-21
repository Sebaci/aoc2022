{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Array (Array, indices, listArray, (!), (//))
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy, last, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

data Monkey = Monkey
  { items :: [Item],
    op :: Item -> Item,
    whereToThrow :: Item -> MonkeyIndex,
    inspections :: Int
  }

type MonkeyIndex = Int

type Item = Int

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  print $ solve 20 $ parse input

solve :: Int -> Array MonkeyIndex Monkey -> Int
solve rounds monkeys = product . take 2 . sortBy (flip compare) $ inspectionsPerMonkey
  where
    inspectionsPerMonkey = map inspections $ toList (gameHistory !! rounds)
    gameHistory = game monkeys

game :: Array MonkeyIndex Monkey -> [Array MonkeyIndex Monkey]
game = iterate playRound

playRound :: Array MonkeyIndex Monkey -> Array MonkeyIndex Monkey
playRound monkeys = foldl playTurn' monkeys (indices monkeys)

playTurn' :: Array MonkeyIndex Monkey -> MonkeyIndex -> Array MonkeyIndex Monkey
playTurn' monkeys monkeyNo = foldl throwItem monkeys (items $ monkeys ! monkeyNo)
  where
    throwItem monkeys item =
      monkeys
        // [ (monkeyNo, curMonkey {items = tail curItems, inspections = inspections + 1}),
             (destMonkeyNo, destMonkey {items = item' : destMonkeyItems})
           ]
      where
        destMonkey@Monkey {items = destMonkeyItems} = monkeys ! destMonkeyNo
        destMonkeyNo = whereToThrow item'
        item' = op item `div` 3

        curMonkey@Monkey {items = curItems, inspections} = monkeys ! monkeyNo

    Monkey {op, whereToThrow} = monkeys ! monkeyNo

parse :: T.Text -> Array MonkeyIndex Monkey
parse input = listArray (0, length monkeys - 1) $ map toMonkey monkeyInput
  where
    monkeys = map toMonkey monkeyInput

    toMonkey [_, start, op, div, iftrue, iffalse] =
      Monkey
        { items = itemList,
          op = toOp $ drop 4 $ words $ T.unpack op,
          whereToThrow = \worry -> if worry `mod` divBy == 0 then monkeyIfTrue else monkeyIfFalse,
          inspections = 0
        }
      where
        itemList = map (read . T.unpack) $ T.split (== ',') (T.takeWhileEnd (/= ':') start)

        monkeyIfTrue = read . T.unpack . last . T.words $ iftrue
        monkeyIfFalse = read . T.unpack . last . T.words $ iffalse
        divBy = read . T.unpack . last . T.words $ div
        toOp ["+", "old"] = (* 2)
        toOp ["*", "old"] = (^ 2)
        toOp ["*", n] = (* read n)
        toOp ["+", n] = (+ read n)
        toOp _ = parseError
    toMonkey arg = parseError

    monkeyInput = filter (\x -> length x > 1) . groupBy ((&&) `on` not . T.null) $ T.lines input

parseError :: a
parseError = error "Could not parse input"
