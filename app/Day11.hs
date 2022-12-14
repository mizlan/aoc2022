{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day11 where

import Control.Monad
import Control.Monad.ST
import Data.Bool
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as S
import Data.Vector.Mutable qualified as MV
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Regex.Applicative (RE (..), (=~))
import Text.Regex.Applicative qualified as R
import Text.Regex.Applicative.Common qualified as R

type Parser = Parsec () String

findAll :: RE s a -> [s] -> [a]
findAll = unfoldr . R.findFirstPrefix . (R.few R.anySym *>)

data Monkey = MkMonkey
  { items :: Seq Int
  , test :: Int
  , proc :: Int -> (Int, Int)
  }

sim :: Int -> [Monkey] -> [Int]
sim rounds xs = runST $ do
  let m = foldl1' lcm $ map test xs
  let n = length xs
  v <- MV.generate n (xs !!)
  counts <- MV.replicate n 0
  replicateM_ rounds $ do
    forM_ [0 .. n - 1] $ \i -> do
      MkMonkey{..} <- v `MV.read` i
      MV.modify counts (+ length items) i
      forM_ items $ \j -> do
        let (j', m') = proc j
        MV.modify v (modifyItems (|> j' `mod` m)) m'
      MV.modify v (modifyItems (const S.empty)) i
  MV.foldr' (:) [] counts
 where
  modifyItems f m = m{items = f m.items}

input :: Bool -> Parser [Monkey]
input divBy3 = monkey `sepBy` string "\n"
 where
  line = many (noneOf (Just '\n')) <* single '\n'
  findDecimal = head . findAll R.decimal <$> line
  monkey = do
    line
    items <- S.fromList . findAll R.decimal <$> line
    opLine <- line
    divBy <- findDecimal
    tMonkey <- findDecimal
    fMonkey <- findDecimal
    let operandRe = (Nothing <$ "old") <|> (pure <$> R.decimal)
        operatorRe =
          R.msym $ \case
            '*' -> pure (*)
            '+' -> pure (+)
            _ -> Nothing
        dotstar = R.few R.anySym
        opRe =
          dotstar *> R.liftA3 (,,) (operandRe <* dotstar) (operatorRe <* dotstar) (operandRe <* dotstar)
    let (lhs, op, rhs) = fromJust $ opLine =~ opRe
    pure $ MkMonkey items divBy $ \n ->
      let n' = (if divBy3 then (`div` 3) else id) $ (op `on` fromMaybe n) lhs rhs
       in (n', bool fMonkey tMonkey (n' `mod` divBy == 0))

solve rounds divBy3 = print . product . take 2 . sortOn Down . sim rounds . fromJust . parseMaybe (input divBy3) =<< readFile "input/day11.1"

solve1 = solve 20 True
solve2 = solve 10000 False
