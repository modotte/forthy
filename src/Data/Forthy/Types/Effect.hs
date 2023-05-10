module Data.Forthy.Types.Effect (Eff (..)) where

import Relude

data Eff = Print | Exit | Emit | Newline deriving (Show, Eq)