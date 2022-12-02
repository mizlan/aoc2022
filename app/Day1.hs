{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.List
import Data.Text (Text (..))
import Data.Text qualified as T

common = do
  xs <- T.splitOn "\n\n" . T.strip . T.pack <$> readFile "./input/day1.1"
  pure $ sum . map (read . T.unpack) . T.splitOn "\n" <$> xs

solve = common >>= print . maximum

solve2 = common >>= print . sum . take 3 . reverse . sort
