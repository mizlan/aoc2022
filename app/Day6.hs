module Day6 where

import Control.Monad (ap)
import Data.List (findIndex, nub, tails)
import Data.Maybe (fromJust)

solve n = print . (+ n) . fromJust . findIndex distinct . windows n =<< readFile "input/day6.1"
 where
  windows n s = map (take n) $ tails s
  distinct = ap (==) nub

solve1 = solve 4
solve2 = solve 14
