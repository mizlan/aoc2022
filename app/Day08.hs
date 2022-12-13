{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NegativeLiterals #-}

module Day08 where

import Data.List (foldl', transpose)
import Data.Set qualified as S
import Data.Set (Set)

ss :: [((Int, Int), Int)] -> [(Int, Int)]
ss xs = fmap (fst . snd) $ filter (\(a, (_, b)) -> a < b) $ zip (scanl max -1 $ snd <$> xs) xs

attach :: [[a]] -> [[((Int, Int), a)]]
attach = mapI (mapI . ((,) .) . (,))
 where
  mapI = flip zipWith [0 ..]

decompose :: [[a]] -> [((Int, Int), a)]
decompose = concat . mapI (\i -> mapI (\j e -> ((i, j), e)))
 where
  mapI = flip zipWith [0 ..]

p2 xs = maximum $ map f xs
  where
    f (p@(i, j), h) = l * r * u * d
      where
        rc = fst . fst
        cc = snd . fst
        row = filter ((== i) . rc) xs
        col = filter ((== j) . cc) xs
        toL = reverse $ filter ((< j) . cc) row
        toR = filter ((> j) . cc) row
        toU = reverse $ filter ((< i) . rc) col
        toD = filter ((> i) . rc) col
        takeWhile' p xs = let (a, b) = span p xs in a ++ take 1 b
        l = length $ takeWhile' (< h) $ snd <$> toL
        r = length $ takeWhile' (< h) $ snd <$> toR
        u = length $ takeWhile' (< h) $ snd <$> toU
        d = length $ takeWhile' (< h) $ snd <$> toD

gs :: [[Int]] -> [[[((Int, Int), Int)]]]
gs g = 
  let g1 = attach g
      g2 = reverse <$> g1
      g3 = transpose g1
      g4 = reverse <$> transpose g1
    in [g1, g2, g3, g4]

s :: [[Int]] -> Set (Int, Int)
s g =
  let gk = gs g
   in S.fromList $ concatMap (concatMap ss) gk

solve = readFile "input/day8.1" >>= print . length . s . map (map (ri . pure)) . lines
  where ri = read :: String -> Int

solve2 = readFile "input/day8.1" >>= print . p2 . decompose . map (map (ri . pure)) . lines
  where ri = read :: String -> Int
