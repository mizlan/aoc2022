{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.Text (Text (..))
import qualified Data.Text as T
import Data.Tree
import Data.Tree.Zipper
import Text.Megaparsec
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parse = Parsec () Text

type Size = Integer
data I = F Text Integer | D Text
  deriving (Show, Eq)

data Command = Cd Text | Ls [I]
  deriving (Show, Eq)

command :: Parse Command
command = do
  string "$ "
  l <- line
  case T.take 2 l of
    "cd" -> pure . Cd $ T.drop 3 l
    "ls" -> Ls <$> lsOutput
 where
  lsOutput = many $ lsDir <|> lsFile
  lsDir = string "dir " *> (D <$> line)
  lsFile = do
    x <- decimal
    string " "
    f <- line
    pure $ F f x
  line = T.pack <$> many (noneOf (Just '\n')) <* single '\n'

parseI :: Text -> [Command]
parseI = fromJust . parseMaybe (many command)

go :: [Command] -> Tree I
go = tree . root . foldl' (flip doCmd) (fromTree $ mkdir "/") . tail
 where
  mkdir s = Node (D s) []
  file f s = Node (F f s) []
  doCmd (Cd "..") = fromJust . parent
  doCmd (Cd dir) = insert (mkdir dir) . children
  doCmd (Ls xs) = modifyTree (\x -> x{subForest = [file f s | F f s <- xs] ++ x.subForest})

dirs :: Tree I -> [Tree I]
dirs r@(Node (D _) xs) = r : concatMap dirs [a | a@(Node (D d) _) <- xs]

sz (Node (F _ s) _) = s
sz (Node (D _) xs) = sum $ sz <$> xs

solve = readFile "input/day7.1" >>= print . minimum . filter (>= 4359867) . map sz . dirs . go . parseI . T.pack
