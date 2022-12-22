module RE (
  module Text.Regex.Applicative,
  module Text.Regex.Applicative.Common,
  findAll,
  replaceFirst,
) where

import Data.List qualified as L
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

findAll :: RE s a -> [s] -> [a]
findAll = L.unfoldr . findFirstPrefix . (few anySym *>)

replaceFirst :: RE s [s] -> [s] -> [s]
replaceFirst r ys = flip (maybe ys) (findLongestInfix r ys) $ \(a, m, b) -> a ++ m ++ b
