{-# LANGUAGE DerivingStrategies #-}

module Stack where

import Data.Vector (Vector)
import Data.Vector qualified as V

newtype Stack = Stack {getStack :: Vector Integer} deriving newtype (Show, Eq)

mkStack :: Stack
mkStack = Stack V.empty
