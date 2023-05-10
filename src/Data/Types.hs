module Data.Types (AppState (..), Op (..), Eff (..), Token (..), ForthyError (..)) where

import Data.Stack.Types (Stack)
import Data.Vector (Vector)
import Relude hiding (Op, Undefined, Word)

data AppState = AppState
  { buffer :: [Text],
    stack :: Stack Integer,
    dictionary :: HashMap Text (Vector Token),
    isInCompileMode :: Bool,
    currentCompileIdentifier :: Maybe Text
  }
  deriving (Show, Eq)

data ForthyError
  = StackUnderflow
  | MissingIdentifier Text
  | InvalidASCIIValue Integer
  deriving (Show, Eq)

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

data Eff = Print | Exit | Emit deriving (Show, Eq)

data Token
  = Effect Eff
  | Operator Op
  | Datum Integer
  | Blank
  | Identifier Text
  deriving (Show, Eq)
