{-# LANGUAGE MultiWayIf #-}

module Day18 where

import Control.Monad.State

import Data.Bifunctor
import Data.List
import Data.Set (Set (..))
import Data.Set qualified as Set
import Text.Regex.Applicative qualified as R
import Text.Regex.Applicative.Common qualified as R

adj6 = [(0, 0, 1), (0, 1, 0), (1, 0, 0), (0, 0, -1), (0, -1, 0), (-1, 0, 0)]
updatePt (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)

type P3 = (Int, Int, Int)

dfs :: Set P3 -> P3 -> State (Int, Set P3) ()
dfs g p@(x, y, z) = forM_ adj6 $ \(updatePt p -> p') -> do
  vis <- gets snd
  if
      | p' `Set.notMember` g -> modify (first (+ 1))
      | p' `Set.member` vis -> pure ()
      | otherwise -> modify (second (Set.insert p')) *> dfs g p'

solve1 =
  inp
    >>= print . fst . flip execState (0, Set.empty) . \input -> forM_ input $ \pt -> do
      vis <- gets snd
      when (pt `Set.notMember` vis) $ modify (second (Set.insert pt)) *> dfs (Set.fromList input) pt

dfs2 :: (P3 -> Bool) -> Set P3 -> P3 -> State (Int, Set P3) ()
dfs2 inDomain g p = forM_ adj6 $ \(updatePt p -> p') -> when (inDomain p') $ do
  vis <- gets snd
  if
      | p' `Set.member` g -> modify (first (+ 1))
      | p' `Set.member` vis -> pure ()
      | otherwise -> modify (second (Set.insert p')) *> dfs2 inDomain g p'

solve2' inp =
  print . fst . flip execState (0, Set.singleton start) $ dfs2 inDomain (Set.fromList inp) start
 where
  start = (minX, minY, minZ)
  minX = minimum [x | (x, _, _) <- inp] - 1
  maxX = maximum [x | (x, _, _) <- inp] + 1
  minY = minimum [y | (_, y, _) <- inp] - 1
  maxY = maximum [y | (_, y, _) <- inp] + 1
  minZ = minimum [z | (_, _, z) <- inp] - 1
  maxZ = maximum [z | (_, _, z) <- inp] + 1
  inDomain (x, y, z) =
    minX <= x
      && x <= maxX
      && minY <= y
      && y <= maxY
      && minZ <= z
      && z <= maxZ

solve2 = solve2' =<< inp

findAll = unfoldr . R.findFirstPrefix . (R.few R.anySym *>)
inp = map parse . lines <$> readFile "input/day18.1"
 where
  parse ln = let [a, b, c] = findAll R.decimal ln in (a, b, c)
