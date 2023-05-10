module Data.Forthy.Operator
  ( handleOps,
  )
where

import Control.Monad.Error.Class (MonadError)
import Data.AppState (AppState)
import Data.AppState qualified as AS
import Data.Bits (Bits ((.|.)), (.&.))
import Data.Bits qualified as DB
import Data.Forthy.Constants qualified as DFC
import Data.Forthy.Types.Error (ForthyError)
import Data.Forthy.Types.Operator
import Data.Stack qualified as S
import Relude hiding (Op, and, first, or, second, swap)

add :: (MonadState AppState m, MonadError ForthyError m) => m ()
add = do
  x <- S.pop
  y <- S.pop
  S.push $ x + y

multiply :: (MonadState AppState m, MonadError ForthyError m) => m ()
multiply = do
  x <- S.pop
  y <- S.pop
  S.push $ x * y

dup :: (MonadState AppState m, MonadError ForthyError m) => m ()
dup = do
  x <- S.pop
  S.push x
  S.push x

swap :: (MonadState AppState m, MonadError ForthyError m) => m ()
swap = do
  x <- S.pop
  y <- S.pop
  S.push x
  S.push y

over :: (MonadState AppState m, MonadError ForthyError m) => m ()
over = do
  first <- S.pop
  second <- S.pop
  S.push second
  S.push first
  S.push second

rot :: (MonadState AppState m, MonadError ForthyError m) => m ()
rot = do
  first <- S.pop
  second <- S.pop
  third <- S.pop
  S.push second
  S.push first
  S.push third

equal :: (MonadState AppState m, MonadError ForthyError m) => m ()
equal = do
  x <- S.pop
  y <- S.pop
  if x == y then S.push DFC.true else S.push DFC.false

invert :: (MonadState AppState m, MonadError ForthyError m) => m ()
invert = do
  x <- S.pop
  S.push $ DB.complement x

or :: (MonadState AppState m, MonadError ForthyError m) => m ()
or = do
  x <- S.pop
  y <- S.pop
  S.push $ x .|. y

and :: (MonadState AppState m, MonadError ForthyError m) => m ()
and = do
  x <- S.pop
  y <- S.pop
  S.push $ x .&. y

largerThan :: (MonadState AppState m, MonadError ForthyError m) => m ()
largerThan = do
  x <- S.pop
  y <- S.pop
  S.push $ if y > x then DFC.true else DFC.false

smallerThan :: (MonadState AppState m, MonadError ForthyError m) => m ()
smallerThan = do
  x <- S.pop
  y <- S.pop
  S.push $ if y < x then DFC.true else DFC.false

handleOps :: (MonadState AppState m, MonadError ForthyError m) => Op -> m ()
handleOps op = do
  s <- get
  case op of
    Add -> add
    Multiply -> multiply
    Dup -> dup
    Drop -> void S.pop
    Swap -> swap
    Over -> over
    Rot -> rot
    Equal -> equal
    Invert -> invert
    Or -> or
    And -> and
    LargerThan -> largerThan
    SmallerThan -> smallerThan
    Fun -> put $ s {AS.isInCompileMode = True}
    EndFun ->
      put $
        s
          { AS.isInCompileMode = False,
            AS.currentCompileIdentifier = Nothing
          }
