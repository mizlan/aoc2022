module Day05 where

import Data.Char (isDigit, isUpper)
import Data.List (foldl', transpose)
import Data.List.Split (splitOn, wordsBy)
import Data.Vector (Vector (..), (!), (//), fromList)

p s func =
  let [g, rq] = lines <$> splitOn "\n\n" s
      k = fromList . filter (not . null) . map (filter isUpper) . transpose $ g
      q = map read . wordsBy (not . isDigit) <$> rq
   in foldl' move k q
 where
  move l [n, subtract 1 -> f, subtract 1 -> t] =
    let (a, b) = splitAt n (l ! f) in l // [(f, b), (t, func a ++ (l ! t))]

s func = readFile "input/day5.1" >>= print . fmap head . flip p func

solve1 = s reverse
solve2 = s id
