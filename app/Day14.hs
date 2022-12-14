module Day14 where

import Data.List
import Data.Maybe
import Data.Set (Set (..))
import Data.Set qualified as S
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parse = Parsec () String

genGraph :: [(Int, Int)] -> Set (Int, Int)
genGraph xs = S.fromList $ concat $ zipWith rng xs (tail xs)
 where
  rng (a, b) (c, d)
    | a == c = zip (repeat c) [min b d .. max b d]
    | otherwise = zip [min a c .. max a c] (repeat b)

simulate2 :: Int -> Set (Int, Int) -> Maybe (Set (Int, Int))
simulate2 fl g = go (500, 0)
 where
  go p@(x, y)
    | p `S.member` g = Nothing
    | y == fl = pure g'
    | (x, y + 1) `S.notMember` g = go (x, y + 1)
    | (x - 1, y + 1) `S.notMember` g = go (x - 1, y + 1)
    | (x + 1, y + 1) `S.notMember` g = go (x + 1, y + 1)
    | otherwise = pure g'
   where
    g' = p `S.insert` g

simulate1 :: Set (Int, Int) -> Maybe (Set (Int, Int))
simulate1 = go (500, 0)
 where
  go p@(x, y) g
    | (x, y) `S.member` g || S.null (S.filter ((> y) . snd) g) = Nothing
    | (x, y + 1) `S.notMember` g = go (x, y + 1) g
    | (x - 1, y + 1) `S.notMember` g = go (x - 1, y + 1) g
    | (x + 1, y + 1) `S.notMember` g = go (x + 1, y + 1) g
    | otherwise = pure $ (x, y) `S.insert` g

s1 :: [String] -> Int
s1 xs = findSand 0 graph
 where
  inp = fromJust . parseMaybe ln <$> xs
  ln = t `sepBy1` string " -> "
  t :: Parse (Int, Int)
  t = do
    x <- decimal
    char ','
    y <- decimal
    pure (x, y)
  graph = foldl1' S.union $ genGraph <$> inp
  fl = (+ 1) . maximum $ map snd $ S.toList graph
  findSand n g = case simulate1 g of
    Nothing -> n
    Just g' -> findSand (n + 1) g'

s2 :: [String] -> Int
s2 xs = findSand 0 graph
 where
  inp = fromJust . parseMaybe ln <$> xs
  ln = t `sepBy1` string " -> "
  t :: Parse (Int, Int)
  t = do
    x <- decimal
    char ','
    y <- decimal
    pure (x, y)
  graph = foldl1' S.union $ genGraph <$> inp
  fl = (+ 1) . maximum $ map snd $ S.toList graph
  findSand n g = case simulate2 fl g of
    Nothing -> n
    Just g' -> findSand (n + 1) g'

solve1 = print . s1 . lines =<< readFile "input/day14.1"
solve2 = print . s2 . lines =<< readFile "input/day14.1"
