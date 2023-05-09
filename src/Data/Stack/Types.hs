{-# LANGUAGE DerivingStrategies #-}

module Data.Stack.Types (Stack (..)) where

import Data.Vector (Vector)
import Relude

newtype Stack a = Stack {unStack :: Vector a} deriving newtype (Show, Eq)