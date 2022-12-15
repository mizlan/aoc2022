{-# LANGUAGE TupleSections #-}

module Day15 where

import Control.Monad
import Data.List hiding ((\\))
import Data.Maybe
import Data.Set (Set (..), (\\))
import Data.Set qualified as S
import Debug.Trace
import Text.Regex.Applicative (RE (..))
import Text.Regex.Applicative qualified as R
import Text.Regex.Applicative.Common qualified as R

d (a, b) (c, d) =
  let [x, y] = sort [a, c]
      [w, z] = sort [b, d]
   in y - x + z - w

findAll :: RE s a -> [s] -> [a]
findAll = unfoldr . R.findFirstPrefix . (R.few R.anySym *>)

p = map ((\[a, b, c, d] -> ((a, b), (c, d))) . findAll (R.signed R.decimal))

data CP = Start {getPos :: Int} | End {getPos :: Int}
  deriving (Show, Eq, Ord)

breakCont' :: [CP] -> Maybe Int
breakCont' xs = go 0 s
 where
  go 1 ((End i) : (Start j) : _) | j - i > 1 = pure $ i + 1
  go depth ((Start i) : xs) = go (depth + 1) xs
  go depth ((End i) : xs) = go (depth - 1) xs
  go _ _ = Nothing
  s = sortOn getPos xs

sensorPicks' :: Int -> ((Int, Int), (Int, Int)) -> [CP]
sensorPicks' k (s@(x, y), p@(a, b)) = if flex >= 0 then ss else []
 where
  totalDist = d s p
  dy = abs (k - y)
  flex = totalDist - dy
  ss = [Start $ x - flex, End $ x + flex]

sensorPicks :: Int -> ((Int, Int), (Int, Int)) -> Set Int
sensorPicks k (s@(x, y), p@(a, b)) = if b == k then ss \\ S.singleton a else ss
 where
  totalDist = d s p
  dy = abs (k - y)
  flex = totalDist - dy
  ss = S.fromList [x - flex .. x + flex]

solve1 = print . S.size . foldl1' S.union . map (sensorPicks 2000000) . p . lines =<< readFile "input/day15.1"
solve2 = do
  inp <- p . lines <$> readFile "input/day15.1"
  let ys = map (snd . fst) inp
  let lb = minimum $ 0 : ys
  let ub = maximum $ 4000000 : ys
  let (x, y) = head $ mapMaybe (\yc -> fmap (,yc) . breakCont' $ concatMap (sensorPicks' yc) inp) [lb .. ub]
  print $ 4000000 * x + y
