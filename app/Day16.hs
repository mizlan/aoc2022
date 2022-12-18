{-# LANGUAGE RecordWildCards #-}

module Day16 where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Chimera
import Data.Chimera.ContinuousMapping
import Data.Foldable
import Data.Functor
import Data.List
import Data.Map (Map (..))
import Data.Map qualified as Map
import Data.Maybe
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set (Set (..))
import Data.Set qualified as Set
import Data.Vector (Vector (..), (!))
import Data.Vector qualified as V
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

findAll = unfoldr . findFirstPrefix . (few anySym *>)
trimap f (a, b, c) = (f a, f b, f c)

bfs :: Ord a => (a -> [a]) -> [a] -> (a -> Bool) -> Map a Int
bfs transition starts end = go Map.empty (Seq.fromList $ zip (repeat 0) starts)
 where
  go vis ((d, x) :<| xs)
    | end x = vis
    | x `Map.member` vis = go vis xs
    | otherwise = go (Map.insert x d vis) (xs >< Seq.fromList (zip (repeat $ d + 1) (transition x)))
  go vis _ = vis

type Graph = Map String Node

data Node = MkNode {tag :: String, flow :: Int, conn :: [String]}
  deriving (Show)

data Node' = MkNode'
  { tag' :: String
  , flow' :: Int
  , conn' :: Map String Int
  }

type Graph' = Map String Node'

cast :: Word -> (Int, Int, Int)
cast = trimap wordToInt . fromZCurve3

uncast :: (Word -> Int) -> (Int -> Int -> Int -> Int)
uncast g x y z = g $ toZCurve3 (intToWord x) (intToWord y) (intToWord z)

dp :: Graph' -> (Word -> Int)
dp graph' = memoizeFix dp'
 where
  ls = V.fromList . zip [0 ..] $ sortOn tag' (Map.elems graph')
  dp' :: (Word -> Int) -> Word -> Int
  dp' (uncast -> f) (cast -> (time, cur, bitmask))
    | time == 0 = 0
    | otherwise = maximum options
   where
    (MkNode' s flow conn) = snd $ ls ! cur
    available = map (ls !) $ filter (not . testBit bitmask) [0 .. 14]
    options = map (\(i, MkNode'{..}) -> f (conn Map.! tag') (idx tag') (setBit bitmask $ idx tag')) available
     where
      idx t = fromJust $ V.findIndex ((== t) . tag' . snd) ls

selected :: Graph -> Graph'
selected g = positiveFlowers <&> (\(MkNode s f c) -> MkNode' s f $ Map.filterWithKey isPositive $ bfs transition [s] (const False))
 where
  positiveFlowers = Map.filter ((> 0) . flow) g
  transition = conn . (g Map.!)
  isPositive = const . (`Map.member` positiveFlowers)

vis = readFile "input/day16.1" >>= print . gl . lines

gl :: [String] -> Graph
gl xs =
  let k = do
        ln <- xs
        let flow = head $ findAll decimal ln
        let (p : ps) = findAll (replicateM 2 (psym isUpper)) ln
        pure (p, MkNode p flow ps)
   in Map.fromList k
