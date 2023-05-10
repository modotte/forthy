module Data.Forthy.Types.Operator (Op (..)) where

import Relude hiding (Op)

data Op
  = Add
  | Multiply
  | Dup
  | Drop
  | Swap
  | Over
  | Rot
  | Equal
  | Invert
  | Or
  | And
  | LargerThan
  | SmallerThan
  | Fun
  | EndFun
  deriving (Show, Eq)