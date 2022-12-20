{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Day16 where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Chimera
import Data.Chimera.ContinuousMapping
import Data.Functor
import Data.List
import Data.Map (Map (..))
import Data.Map qualified as Map
import Data.Maybe
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set (Set (..))
import Data.Set qualified as Set
import Data.Tuple.Extra
import Data.Vector ((!))
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

data Node = MkNode {tag :: String, flow :: Int, conn :: [String]}

type Graph = Map String Node

data Node' = MkNode'
  { tag' :: String
  , flow' :: Int
  , conn' :: Map String Int
  }

type Graph' = Map String Node'

cast :: Word -> (Int, Int, Int)
cast = trimap wordToInt . fromZCurve3

uncast :: (Word -> Int) -> (Int -> Int -> Int -> Int)
uncast g = curry3 $ g . uncurry3 toZCurve3 . trimap intToWord

dp :: Graph' -> Int -> Int -> Int -> Int
dp graph' = uncast $ memoizeFix dp'
 where
  ls = V.fromList . zip [0 ..] $ sortOn tag' (Map.elems graph')
  dp' (uncast -> f) (cast -> (time, cur, bitmask))
    | time == 30 || null options = 0
    | otherwise = maximum options
   where
    (MkNode' s flow conn) = snd $ ls ! cur
    available = map (ls !) $ filter (not . testBit bitmask) [1 .. 15]
    options =
      mapMaybe
        ( \(i, MkNode'{..}) ->
            let time' = time + conn Map.! tag' + 1
             in if time' > 30
                  then Nothing
                  else pure $ (30 - time') * flow' + f time' (idx tag') (setBit bitmask $ idx tag')
        )
        available
     where
      idx t = fromJust $ V.findIndex ((== t) . tag' . snd) ls

selected :: Graph -> Graph'
selected g = starts <&> (\(MkNode s f c) -> MkNode' s f $ Map.filterWithKey isPositive $ bfs transition [s] (const False))
 where
  positiveFlowers = Map.filter ((> 0) . flow) g
  starts = Map.singleton "AA" (g Map.! "AA") `Map.union` positiveFlowers
  transition = conn . (g Map.!)
  isPositive = const . (`Map.member` positiveFlowers)

gl :: [String] -> Graph
gl xs = Map.fromList do
  ln <- xs
  let flow = head $ findAll decimal ln
  let (p : ps) = findAll (replicateM 2 (psym isUpper)) ln
  pure (p, MkNode p flow ps)

solve1' inp =
  let graph = gl $ lines inp
      dpF = dp $ selected graph
   in dpF 0 0 0

solve1 = readFile "input/day16.1" >>= print . solve1'
