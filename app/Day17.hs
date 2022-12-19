{-# LANGUAGE PostfixOperators #-}

module Day17 where

import Data.Foldable
import Data.List.Extra
import Data.Ord
import Data.Set (Set (..))
import Data.Set qualified as S

shapes =
  [ [(0, 0), (1, 0), (2, 0), (3, 0)]
  , [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
  , [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
  , [(0, 0), (0, 1), (0, 2), (0, 3)]
  , [(0, 0), (0, 1), (1, 0), (1, 1)]
  ]

(.-) x = (x, 0)
(.|) y = (0, y)
(|.) s l = s `S.union` S.fromList l

instance (Num a, Num b) => Num (a, b) where
  (a, b) + (c, d) = (a + c, b + d)

type V2 = (Int, Int)

-- env ! i shall not be empty
simulate :: (String, Set V2, [[V2]]) -> (String, Set V2, [[V2]])
simulate (jets, env, shape : ss) = go jets env (2, highest + 4)
 where
  highest = maximum $ map snd $ S.toList env
  go (j : js) env p@(x, y) = case (all isValid jetPts, all isValid (down <$> jetPts), all isValid (down <$> noJetPts)) of
    (True, True, _) -> go js env (down p')
    (True, False, _) -> (js, env |. jetPts, ss)
    (False, _, True) -> go js env (down p)
    _ -> (js, env |. noJetPts, ss)
   where
    p'@(x', y') = p + if j == '<' then (-1 .-) else (1 .-)
    jetPts = (+ p') <$> shape
    noJetPts = (+ p) <$> shape
    isValid (x, y) = 0 <= x && x < 7 && (x, y) `S.notMember` env
    down = (+ (-1 .|))

s1 jets =
  let (_, v, _) = (!! 2022) . iterate simulate $ (cycle jets, S.fromAscList $ zip [0 .. 6] [0, 0 ..], cycle shapes)
   in v

inp = filter (`elem` "<>") <$> readFile "input/day17.1"

shower xs = map (\n -> if n `elem` xs then '#' else '.') [0 .. 100]

solve1 = do
  -- pure ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>" >>= traverse_ print . reverse . transpose . fmap shower . fmap (take 10000 . reverse) . V.toList . s1
  -- inp >>= traverse_ print . reverse . transpose . fmap shower . fmap (take 10000 . reverse) . S.toList . s1
  inp >>= print . maximum . map snd . S.toList . s1

-- (print . maximum . map snd . S.toList . s1) ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
