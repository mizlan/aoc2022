{-# LANGUAGE ApplicativeDo #-}

-- today's was a bit different: my haskell code outputs more code to be evaluated
-- since haskell doesn't have a proper symbolic math library (that works on arm64),
-- i resort to outputting python code for part 2 to use with sympy

module Day21 where

import Data.List
import Hydra
import RE
import Text.Printf

ident = some (psym (`elem` ['a' .. 'z']))
human = "humn = lambda: sympy.Symbol('x')" <$ (string "humn =" *> many anySym)
functionize = (<> "()") <$> ident
colon = " = lambda: " <$ string ": "
notDecl = "" <$ string "()"
root = do
  begin <- string "root = lambda: "
  lhs <- ident <> string "()"
  string " "
  psym (`elem` "+-/*")
  string " "
  rhs <- ident <> string "()"
  pure $ begin <> printf "%s - %s" lhs rhs
lazify = replaceFirst notDecl . replace colon . replace functionize

solve1 = do
  file <- lines <$> readFile "input/day21.1"
  let file' = lazify <$> file
  runPython (unlines $ file' <> ["print(int(root()))"]) >>= putStrLn

solve2 = do
  file <- lines <$> readFile "input/day21.1"
  let file' = replace root . replace human . lazify <$> file
  runPython (unlines $ ["import sympy"] <> file' <> ["print(int(sympy.solve(root(), humn())[0]))"]) >>= putStrLn
