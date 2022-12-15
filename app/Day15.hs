module Day15 where

import Data.List hiding ((\\))
import Data.Maybe
import Data.Set (Set (..), (\\))
import Data.Set qualified as S
import Text.Regex.Applicative qualified as R
import Text.Regex.Applicative.Common qualified as R

findAll :: R.RE s a -> [s] -> [a]
findAll = unfoldr . R.findFirstPrefix . (R.few R.anySym *>)

data IntervalEndpoint = Start {getPos :: Int} | End {getPos :: Int}
  deriving (Show, Eq, Ord)

firstGap :: [IntervalEndpoint] -> Maybe Int
firstGap xs = go 0 $ sortOn getPos xs
 where
  go 1 ((End i) : (Start j) : _) | j - i > 1 = pure $ i + 1
  go depth ((Start i) : xs) = go (depth + 1) xs
  go depth ((End i) : xs) = go (depth - 1) xs
  go _ _ = Nothing

flex k s@(x, y) p@(a, b) = abs (x - a) + abs (y - b) - abs (k - y)

sensorPicks :: Int -> ((Int, Int), (Int, Int)) -> Set Int
sensorPicks k (s@(x, y), p@(a, b))
  | b == k = ss \\ S.singleton a
  | otherwise = ss
 where
  f = flex k s p
  ss = S.fromList [x - f .. x + f]

sensorPicks' :: Int -> ((Int, Int), (Int, Int)) -> [IntervalEndpoint]
sensorPicks' k (s@(x, y), p@(a, b))
  | f >= 0 = [Start $ x - f, End $ x + f]
  | otherwise = []
 where
  f = flex k s p

parseInput = map ((\[a, b, c, d] -> ((a, b), (c, d))) . findAll (R.signed R.decimal))

solve1 = print . S.size . foldl1' S.union . map (sensorPicks 2000000) . parseInput . lines =<< readFile "input/day15.1"
solve2 = do
  inp <- parseInput . lines <$> readFile "input/day15.1"
  let ys = map (snd . fst) inp
  let lb = minimum ys
      ub = maximum ys
      (x, y) = head $ mapMaybe (\yc -> fmap (,yc) . firstGap $ concatMap (sensorPicks' yc) inp) [lb .. ub]
  print $ 4000000 * x + y
