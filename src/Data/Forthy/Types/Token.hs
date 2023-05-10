module Data.Forthy.Types.Token (Token (..)) where

import Data.Forthy.Types.Effect (Eff)
import Data.Forthy.Types.Operator (Op)
import Relude hiding (Op)

data Token
  = Effect Eff
  | Operator Op
  | Datum Integer
  | Blank
  | Identifier Text
  deriving (Show, Eq)