module Day15pt2Fast where

import Data.List
import Text.Regex.Applicative qualified as R
import Text.Regex.Applicative.Common qualified as R

data Sensor = MkSensor {pos :: (Int, Int), radius :: Int}
  deriving (Show)

-- parsing code
findAll = unfoldr . R.findFirstPrefix . (R.few R.anySym *>)
parseInput = map ((\[a, b, c, d] -> MkSensor (a, b) $ dist (a, b) (c, d)) . findAll (R.signed R.decimal))

dist (x, y) (a, b) = abs (x - a) + abs (y - b)

sensorLines :: Sensor -> [(Int, Int)]
sensorLines (MkSensor (x, y) (succ -> r)) = [(0, y - x + r), (1, y + x - r), (2, y - x - r), (3, y + x + r)]

beacon sensors = head [p | (m2 -> a, y1) <- lns, (m2 -> b, y2) <- lns, y1 < y2 && a == 0 && b == 1, let p = point y1 y2, covered p]
 where
  lns = sensors >>= sensorLines
  m2 = (`mod` 2)
  covered p = (== [0 .. 3]) . sort . map fst $ filter (`hasPoint` p) lns
  point y1 y2 = let m = (y2 - y1) `div` 2 in (m, y1 + m)
  hasPoint (m2 -> a, y') (x, y) = a == 0 && y - x == y' || a == 1 && y + x == y'

-- the following is unneeded (for my input at least) but it in theory should
-- be an extra condition in the filter in the case that there are sensor
-- boundary lines that happen to intersect but not the line *segments* themselves
--
-- far p MkSensor{..} = dist pos p > radius

solve2 = do
  inp <- parseInput . lines <$> readFile "input/day15.1"
  let (x, y) = beacon inp
  print $ 4000000 * x + y
