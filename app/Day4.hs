{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PostfixOperators #-}

module Day4 where

import Control.Applicative hiding (many, some)
import Data.Text (Text (..))
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe (fromJust)

type Parser = Parsec () Text

type D = [((Int, Int), (Int, Int))]

(~) = flip

input :: Parser D
input = p `sepBy` newline
  where
    g = do
      a <- decimal
      char '-'
      b <- decimal
      pure (a, b)
    p = do
      a <- g
      char ','
      b <- g
      pure (a, b)

s1 :: D -> Int
s1 = length . filter (\((a, b), (c, d)) -> a ~> (c, d) || b ~> (c, d) || c ~> (a, b) || d ~> (a, b))
  where a ~> (c, d) = c <= a && a <= d

inp = T.strip . T.pack <$> readFile "./input/day4.1"

solve = inp >>= print . s1 . fromJust . parseMaybe input
