module Data.AppState (AppState (..)) where

import Data.Forthy.Types.Token (Token)
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
