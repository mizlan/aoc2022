{-# LANGUAGE PostfixOperators #-}

module Day03 where

import Data.List.Extra (maximumOn)
import Data.Maybe (mapMaybe)
import Data.Set (fromAscList, intersection, findMin, fromList)
import Data.List.Split (chunksOf)

(~) = flip

score = (lookup~) $ zip ['A'..'Z'] [27..52] ++ zip ['a'..'z'] [1..26]

s1 s = let (a, b) = splitAt (length s `div` 2) s in score $ maximumOn (`elem` b) a

s2 = score . findMin . foldl1 intersection . fmap fromList

solve = readFile "./input/day3.1" >>= print . sum . mapMaybe s1 . lines

solve2 = readFile "./input/day3.1" >>= print . sum . mapMaybe s2 . chunksOf 3 . lines
