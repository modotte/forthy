module Data.Stack (module Data.Stack, empty, push, pop) where

import Data.Bifunctor qualified as B
import Data.Stack.Types (Stack (..))
import Data.Stack.Types qualified as ST
import Data.Types (AppState)
import Data.Types qualified as DT
import Data.Vector qualified as V
import Relude hiding (empty, state)
import Relude.Extra qualified as RE

empty :: Stack
empty = ST.Stack V.empty

addStack :: Integer -> Stack -> Stack
addStack x = RE.under $ V.cons x

push :: Integer -> Stack -> Stack
push x s = s

pushM :: (MonadState AppState m) => Integer -> m ()
pushM x =
  modify' $ \state ->
    let stack' = addStack x $ DT.stack state
     in state {DT.stack = stack'}

pop :: Stack -> Maybe (Integer, Stack)
pop (ST.Stack s) =
  case V.uncons s of
    Nothing -> Nothing
    Just x -> Just $ B.second ST.Stack x