module Day13 where

import Data.List
import Data.List.Extra
import Text.RegexPR

data P = I Int | L [P]
  deriving (Eq, Read)

instance Ord P where
  compare (I a) (I b) = compare a b
  compare (L a) (L b) = compare a b
  compare (I a) b = compare (L [I a]) b
  compare a (I b) = compare a (L [I b])

fmt = gsubRegexPRBy "\\d+" ("I " <>) . replace "[" "L ["
rp = read :: String -> P
solve1 = print . sum . map (+ 1) . findIndices (\[rp -> a, rp -> b] -> a < b) . map lines . splitOn "\n\n" . fmt =<< readFile "input/day13.1"
solve2 = print . product . map (+ 1) . findIndices (`elem` d) . sort . (<> d) . map rp . filter notNull . lines . fmt =<< readFile "input/day13.1"
 where
  d = [L [L [I 2]], L [L [I 6]]]
