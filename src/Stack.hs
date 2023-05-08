{-# LANGUAGE DerivingStrategies #-}

module Stack (Stack (..), mkStack) where

import Data.Vector (Vector)
import Data.Vector qualified as V

newtype Stack = Stack {unStack :: Vector Integer} deriving newtype (Show, Eq)

mkStack :: Stack
mkStack = Stack V.empty
