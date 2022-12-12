module Day12 where

import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set (Set (..))
import Data.Set qualified as Set
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Bifunctor (bimap)
import Data.Char (ord)

grid :: IO (Vector (Vector Char))
grid = fmap V.fromList . V.fromList . lines <$> readFile "input/day12.1"

g !* (i, j) = g ! i ! j

neighbors grid p@(i, j) =
  let news = map (($ p) . uncurry bimap) [((- 1), id), (id, (- 1)), ((+ 1), id), (id, (+ 1))]
   in filter chk news
 where
  n = length grid
  m = length (grid ! 0)
  c = grid ! i ! j
  elevation 'S' = ord 'a'
  elevation 'E' = ord 'z'
  elevation c = ord c
  chk p@(i, j) = i >= 0 && i < n && j >= 0 && j < m && elevation (grid !* p) - elevation c <= 1

bfs :: Ord a => (a -> [a]) -> [a] -> (a -> Bool) -> Maybe Int
bfs transition starts end = go Set.empty (Seq.fromList $ zip (repeat 0) starts)
 where
  go vis ((d, x) :<| xs)
    | end x = pure d
    | x `Set.member` vis = go vis xs
    | otherwise = go (x `Set.insert` vis) (xs >< Seq.fromList (zip (repeat $ d + 1) (transition x)))
  go vis _ = Nothing

findStart p g =
  let n = length g
      m = length (g ! 0)
   in filter (p . (g !*)) ((,) <$> [0 .. n - 1] <*> [0 .. m - 1])

solve sp = do
  g <- grid
  let s = findStart sp g
  print $ bfs (neighbors g) s ((== 'E') . (g !*))

solve1 = solve (== 'S')
solve2 = solve (`elem` "Sa")
