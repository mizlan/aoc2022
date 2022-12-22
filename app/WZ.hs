{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WZ (
  WZ (..),
  left,
  dragLeft,
  right,
  dragRight,
  runWZ,
  modify,
  toIndex,
  start,
  end,
  scope,
  get,
  toList,
) where

import Control.Arrow (second)
import Control.Monad.State hiding (get, modify)
import Control.Monad.State qualified as State
import Data.Coerce
import WrapZip (WrapZip (..))
import WrapZip qualified as WZI

newtype WZ e a = WZ {unWZ :: State (WrapZip e) a}
  deriving (Functor, Applicative, Monad, MonadState (WrapZip e))

runWZ :: [e] -> WZ e a -> (a, [e])
runWZ init wz = second WZI.toList $ runState (unWZ wz) (WZI.fromList init)

get :: WZ e e
get = gets WZI.value

left :: WZ e ()
left = State.modify WZI.left

right :: WZ e ()
right = State.modify WZI.right

dragLeft :: WZ e ()
dragLeft = State.modify WZI.dragLeft

dragRight :: WZ e ()
dragRight = State.modify WZI.dragRight

start :: WZ e ()
start = State.modify WZI.start

end :: WZ e ()
end = State.modify WZI.end

toIndex :: Int -> WZ e ()
toIndex = State.modify . WZI.toIndex

scope :: (e -> Bool) -> WZ e ()
scope p = State.modify $ WZI.scope p

modify :: (e -> e) -> WZ e ()
modify = State.modify . WZI.modifyValue

toList :: WZ e [e]
toList = State.gets WZI.toList
