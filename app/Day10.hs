module Day10 where

import Data.List (foldl')
import Data.List.Split (chunksOf)

ps s = reverse p
 where
  p = foldl' k [1] $ words <$> s
  k l@(x : _) ["addx", read -> n] = x + n : x : l
  k l@(x : _) _ = x : l

s1 s = sum $ zipWith score [1 ..] $ ps s
 where
  score i e = if i `elem` [20, 60 .. 220] then i * e else 0

s2 s = unlines . chunksOf 40 . zipWith pix [0 ..] $ ps s
 where
  pix i e
    | abs (i `mod` 40 - e) <= 1 = '#'
    | otherwise = '.'

solve1 = readFile "input/day10.1" >>= print . s1 . lines
solve2 = readFile "input/day10.1" >>= putStrLn . s2 . lines
