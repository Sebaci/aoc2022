module Main where

import Data.List (mapAccumL)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

data Instruction = Noop | Addx Int

data Pixel = Lit | Dark

main :: IO ()
main = do
  args <- getArgs
  input <- T.readFile $ head args
  mapM_ print (solve $ parse input)

solve :: [Instruction] -> [T.Text]
solve = toImage . generatePixels

generatePixels :: [Instruction] -> [Pixel]
generatePixels instructions = snd $ mapAccumL aux 1 (zip [0 ..] modifyOps)
  where
    aux reg (index, op) = (op reg, selectPixel (index `mod` 40) reg)
    modifyOps = concatMap toModifyOp instructions

    toModifyOp Noop = [id]
    toModifyOp (Addx n) = [id, (+ n)]

    selectPixel position register = if abs (position - register) <= 1 then Lit else Dark

toImage :: [Pixel] -> [T.Text]
toImage pixels = T.chunksOf 40 (T.pack (map toChar pixels))
  where
    toChar Lit = '#'
    toChar Dark = '.'

parse :: T.Text -> [Instruction]
parse = map (toInstruction . T.unpack) . T.lines
  where
    toInstruction "noop" = Noop
    toInstruction instr = case words instr of
      ["addx", v] -> Addx $ read v
      _ -> error "Unknown instruction!"
