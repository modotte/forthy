module Data.Types (AppState (..), Op (..), Eff (..), Token (..), ForthyError (..)) where

import Data.Stack.Types (Stack)
import Relude hiding (Op, Undefined, Word)

data AppState = AppState
  { buffer :: [Text],
    stack :: Stack
  }
  deriving (Show, Eq)

data ForthyError = StackUnderflow deriving (Show, Eq)

data Op = Add | Multiply | Dup deriving (Show, Eq)

data Eff = Print | Exit deriving (Show, Eq)

data Token = Effect Eff | Operator Op | Datum Integer | Blank deriving (Show, Eq)