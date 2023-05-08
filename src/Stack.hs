{-# LANGUAGE DerivingStrategies #-}

module Stack (Stack, empty, push, pop) where

import Data.Bifunctor qualified as B
import Data.Vector (Vector)
import Data.Vector qualified as V

newtype Stack = Stack {unStack :: Vector Integer} deriving newtype (Show, Eq)

empty :: Stack
empty = Stack V.empty

push :: Integer -> Stack -> Stack
push x (Stack s) = Stack $ V.cons x s

pop :: Stack -> Maybe (Integer, Stack)
pop (Stack s) =
  case V.uncons s of
    Nothing -> Nothing
    Just x -> Just $ B.second Stack x