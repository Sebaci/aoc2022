# aoc2022
Advent of Code 2022 solutions
https://adventofcode.com/2022

## Basic assumptions

- language: Haskell
- using GHC libraries only; no external libraries
- every hs file is a standalone solution (e.g. "1b.hs" refers to the second part of task 1)
- input is read from file given as an argument (e.g. ```./1b inputFile```)
- various approaches: while optimum solutions are preferred, effectiveness may be sacrificed for the sake of simpler/concise/elegant/fancy result; multiple solutions for one task may appear

## Discussion

#### Day 1: Calorie Counting

Ordinary usage of list operations. With a bit more code could be sped up by extracting the greatest values in a different way, e.g. using specific reduce.

#### Day 2: Rock Paper Scissors

Quite explicit implementation

#### Day 3: Rucksack Reorganization

Rucksacks are modeled as a pair of compartments, which are strings. Since strings are char arrays, we can use intersect to determine repeating items (chars).

In the second part, rucksacks are parsed directly to groups of 3, then intersection is applied.

#### Day 4: Camp Cleanup

To check containment in both ways, `contains` is applied in ordinary and flipped manner; `(||)` is performed on the results in applicative manner - a trick to avoid explicitly mentionint the argument

#### Day 5: Supply Stacks

The stack is parsed by first mapping each line to chunks of 4 chars, from which the second letter is extracted. Stacks are vertical, so transposing the array and eliminatind empty values is enough to achieve proper stacks representation. Array representation is convinient in further manipulation. Elements are moved explicitly one by one using recursion.

In the second part just concatenation was used since the moved part is not reversed.

Fold is used to perform instructions. Since it expects two more arguments, appropiate usage of composition is applied in point-free style.

#### Day 6: Tuning Trouble

#### Day 7: No Space Left On Device

#### Day 8: Treetop Tree House

#### Day 9: Rope Bridge

#### Day 10: Cathode-Ray Tube

#### Day 11: Monkey in the Middle

#### Day 12: Hill Climbing Algorithm

#### Day 13: Distress Signal

#### Day 14: Regolith Reservoir

#### Day 15: Beacon Exclusion Zone

