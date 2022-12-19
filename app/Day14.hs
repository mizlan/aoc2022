{-# LANGUAGE TupleSections #-}

module Day14 where

import Data.List
import Data.Maybe
import Data.Set (Set (..))
import Data.Set qualified as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec () String

options (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

simulate2 stop g = go (500, 0)
 where
  go p@(x, y)
    | p `S.member` g = Nothing
    | stop y g = pure g'
    | otherwise = maybe (pure g') go $ find (`S.notMember` g) (options p)
   where
    g' = p `S.insert` g

simulate1 :: Set (Int, Int) -> Maybe (Set (Int, Int))
simulate1 g = go (500, 0)
 where
  go p@(x, y)
    | (x, y) `S.member` g || S.null (S.filter ((> y) . snd) g) = Nothing
    | otherwise = maybe (pure $ p `S.insert` g) go $ find (`S.notMember` g) (options p)

s1 = sum . unfoldr (fmap (1,) . simulate1)

s2 graph = sum $ unfoldr (fmap (1,) . simulate2 (const . (== fl))) graph
 where
  fl = (+ 1) . maximum $ map snd $ S.toList graph

p :: [String] -> Set (Int, Int)
p xs = graph
 where
  graph = S.unions $ map genGraph inp
  inp = fromJust . parseMaybe ln <$> xs
  ln = t `sepBy1` string " -> "
  t :: Parser (Int, Int)
  t = (,) <$> decimal <* char ',' <*> decimal
  genGraph xs = S.fromList . concat $ zipWith rng xs (tail xs)
  rng (a, b) (c, d)
    | a == c = zip (repeat c) [min b d .. max b d]
    | otherwise = zip [min a c .. max a c] (repeat b)

common s = print . s . p . lines =<< readFile "input/day14.1"
solve1 = common s1
solve2 = common s2
