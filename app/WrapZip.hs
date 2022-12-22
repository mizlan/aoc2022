{-# LANGUAGE RecordWildCards #-}

module WrapZip where

import Control.Monad.State
import Data.List.Extra hiding (length)

-- import Data.List.NonEmpty qualified as NE
-- import Data.List.NonEmpty.Extra qualified as NE

data WrapZip a = MkWrapZip {lefts :: [a], value :: a, rights :: [a]}
  deriving (Eq, Show)

left z@(MkWrapZip [] _ []) = z
left z@(MkWrapZip{lefts = [], rights = reverse -> (x : xs), ..}) = MkWrapZip (xs `snoc` value) x []
left (MkWrapZip{lefts = (x : xs), ..}) = MkWrapZip xs x $ value : rights

right z@(MkWrapZip [] _ []) = z
right z@(MkWrapZip{rights = [], lefts = reverse -> (x : xs), ..}) = MkWrapZip [] x (xs `snoc` value)
right (MkWrapZip{rights = rights@(x : xs), ..}) = MkWrapZip (value : lefts) x xs

dragLeft z@(MkWrapZip [] v r)
  | (x : xs) <- reverse r = MkWrapZip xs v [x]
  | otherwise = z
dragLeft (MkWrapZip{lefts = (x : xs), ..}) = MkWrapZip xs value (x : rights)

dragRight z@(MkWrapZip l v [])
  | (x : xs) <- reverse l = MkWrapZip [x] v xs
  | otherwise = z
dragRight (MkWrapZip{rights = (x : xs), ..}) = MkWrapZip (x : lefts) value xs

toIndex i = fmap snd . fromListScope ((== i) . fst) . zip [0 ..] . toList

length (MkWrapZip{..}) = Prelude.length lefts + Prelude.length rights + 1

start = toIndex 0
end wz = let n = WrapZip.length wz in toIndex n wz

toList (MkWrapZip{..}) = reverse lefts ++ value : rights

modifyValue f (MkWrapZip{..}) = MkWrapZip lefts (f value) rights

scope p = fromListScope p . toList

fromList [] = error "unsupported"
fromList (x : xs) = MkWrapZip [] x xs
fromListScope p xs = case break p xs of
  (a, []) -> error "nothing"
  (a, x : xs) -> MkWrapZip (reverse a) x xs

instance Functor WrapZip where
  fmap f (MkWrapZip{..}) = MkWrapZip (f <$> lefts) (f value) (f <$> rights)
