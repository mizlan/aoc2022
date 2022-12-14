module Day09 where

import Data.List
import Data.Set qualified as S

data Dir = L | R | U | D
  deriving (Read)

s n = S.size . fst . foldl' move (S.singleton o, replicate n o)
 where
  o = (0, 0)
  move (v, (hi, hj) : t) c = (last t' `S.insert` v, t')
   where
    new p@(i', j') c@(i, j)
      | max (abs (i - i')) (abs (j - j')) <= 1 = c
      | otherwise = (i + signum (i' - i), j + signum (j' - j))
    t' = take (1 + length t) $ flip (scanl' new) t $ case c of
      L -> (hi, hj - 1)
      R -> (hi, hj + 1)
      U -> (hi + 1, hj)
      D -> (hi - 1, hj)

input = concatMap ((\[a, b] -> replicate (read b) $ read a) . words) . lines <$> readFile "input/day9.1"

solve1 = print . s 2 =<< input
solve2 = print . s 10 =<< input
