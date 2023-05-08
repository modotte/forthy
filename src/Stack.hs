module Stack (empty, push, pop) where

import Data.Bifunctor qualified as B
import Data.Vector (Vector)
import Data.Vector qualified as V
import Relude.Extra qualified as RE
import Types (Stack(..))

empty :: Stack
empty = Stack V.empty

addStack :: Integer -> Stack -> Stack
addStack x = RE.under (V.cons x)

push :: Integer -> Stack -> Stack
push x (Stack s) = Stack $ V.cons x s

pop :: Stack -> Maybe (Integer, Stack)
pop (Stack s) =
  case V.uncons s of
    Nothing -> Nothing
    Just x -> Just $ B.second Stack x