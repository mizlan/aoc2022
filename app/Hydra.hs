module Hydra where

import Data.Text qualified as Text
import System.Process
import Control.Monad

-- page :: String -> IO ()
-- page s = do k <- system "less" s; pure ()

page = pure

runHaskell :: String -> IO String
runHaskell = readCreateProcess (shell "runhaskell")

prunHaskell :: String -> IO String
prunHaskell s = page s *> runHaskell s

runPython :: String -> IO String
runPython = readCreateProcess (shell "/Users/ml/GlobalVenv/bin/python")

prunPython :: String -> IO String
prunPython s = page s *> runPython s
