module Day10 where

import Data.List (foldl')

s1 s = sum $ zipWith score [1 ..] $ reverse p
 where
  r ["addx", read -> n] = \x -> [x + n, x]
  r ["noop"] = pure
  score i e = if i `elem` [20, 60 .. 220] then i * e else 0
  p = foldl' (\l@(x : xs) a -> a x ++ l) [1] (r . words <$> s)

solve1 = readFile "input/day10.1" >>= print . s1 . lines
