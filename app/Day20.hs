module Day20 where

import Control.Monad
import Debug.Trace
import WZ (WZ (..))
import WZ qualified

s1 :: [Int] -> [Int]
s1 (map (* 811589153) -> xs) =
  let indexed = zip [0 ..] xs
      n = length xs
   in map snd . snd . WZ.runWZ indexed . replicateM_ 10 $ do
        l <- WZ.toList
        traceShowM $ snd <$> l
        forM_ indexed $ \(i, _) -> do
          WZ.scope ((== i) . fst)
          (_, v) <- WZ.get
          let v' = abs v `mod` n
          traceShowM (v, v')
          replicateM_ (abs v `mod` n) (if v < 0 then WZ.dragLeft else WZ.dragRight)
        l <- WZ.toList
        traceShowM $ snd <$> l

groveCoords :: WZ Int Int
groveCoords = do
  WZ.scope (== 0)
  replicateM_ 1000 WZ.right
  x <- WZ.get
  replicateM_ 1000 WZ.right
  y <- WZ.get
  replicateM_ 1000 WZ.right
  z <- WZ.get
  pure $ x + y + z

solve1 = readFile "input/k" >>= print . fst . flip WZ.runWZ groveCoords . s1 . map read . lines
