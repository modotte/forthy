module Data.Types.Error (ForthyError (..)) where

import Relude

data ForthyError
  = StackUnderflow
  | MissingIdentifier Text
  | InvalidASCIIValue Integer
  deriving (Show, Eq)