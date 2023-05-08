{-# LANGUAGE DerivingStrategies #-}

module Types (Stack (..), AppState (..), Op (..), Eff (..), Token (..), ForthyError (..)) where

import Data.Vector (Vector)
import Relude hiding (Op, Undefined, Word)

newtype Stack = Stack {unStack :: Vector Integer} deriving newtype (Show, Eq)

data AppState = AppState
  { buffer :: [Text],
    stack :: [Stack]
  }
  deriving (Show, Eq)

data Op = Add | Multiply | Dup deriving (Show, Eq)

data Eff = Print | Exit deriving (Show, Eq)

data Token = Effect Eff | Operator Op | Datum Integer | Blank deriving (Show, Eq)

data ForthyError = StackUnderflow deriving (Show, Eq)