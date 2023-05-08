module Data.Stack (module Data.Stack, module Data.Stack.Types) where

import Data.Bifunctor qualified as B
import Data.Vector qualified as V
import Relude hiding (empty, state)
import Relude.Extra qualified as RE
import Data.Stack.Types

empty :: Stack
empty = Stack V.empty

addStack :: Integer -> Stack -> Stack
addStack x = RE.under (V.cons x)

push :: Integer -> Stack -> Stack
push x s = s

pop :: Stack -> Maybe (Integer, Stack)
pop (Stack s) =
  case V.uncons s of
    Nothing -> Nothing
    Just x -> Just $ B.second Stack x