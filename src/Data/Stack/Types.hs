{-# LANGUAGE DerivingStrategies #-}

module Data.Stack.Types (Stack (..)) where

import Data.Vector (Vector)
import Relude

newtype Stack = Stack {unStack :: Vector Integer} deriving newtype (Show, Eq)