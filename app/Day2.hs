{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day2 where

import Data.List
import Data.Text (Text (..))
import Data.Text qualified as T
import Control.Arrow

to2 [a, b] = (a, b)

data RPS = R | P | S
  deriving (Show, Eq, Ord, Enum)

data Result = L | T | W
  deriving (Show, Eq, Ord, Enum)

decode :: Text -> RPS
decode "A" = R
decode "X" = R
decode "B" = P
decode "Y" = P
decode "C" = S
decode "Z" = S

res R S = L
res S P = L
res P R = L
res x y
  | x == y = T
  | otherwise = W

win R = P
win P = S
win S = R

lose = win . win

tie = id

decode' :: Text -> (RPS -> RPS)
decode' "X" = lose
decode' "Y" = tie
decode' "Z" = win

score :: RPS -> Int
score = (+ 1) . fromEnum

score' :: Result -> Int
score' = (* 3) . fromEnum

solve = do
  xs <- map (to2 . map decode . T.words) . T.lines . T.strip . T.pack <$> readFile "./input/day2.1"
  print $ sum $ uncurry (+) . ((score . snd) &&& (score' . uncurry res)) <$> xs

solve2 = do
  xs <- map ((\(a, b) -> (decode a, decode' b (decode a))) . to2 . T.words) . T.lines . T.strip . T.pack <$> readFile "./input/day2.1"
  print $ sum $ uncurry (+) . ((score . snd) &&& (score' . uncurry res)) <$> xs
