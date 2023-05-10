module Data.Stack (module Data.Stack.Types, empty, push, pop) where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.AppState (AppState)
import Data.AppState qualified as AS
import Data.Forthy.Types.Error (ForthyError)
import Data.Forthy.Types.Error qualified as DFTE
import Data.Stack.Types
import Data.Stack.Types qualified as ST
import Data.Vector qualified as V
import Relude hiding (empty, print, state)

empty :: Stack a
empty = ST.Stack V.empty

addStack :: a -> Stack a -> Stack a
addStack x (ST.Stack s) = ST.Stack $ V.snoc s x

push :: MonadState AppState m => Integer -> m ()
push x =
  modify' $ \state ->
    let stack = addStack x $ AS.stack state
     in state {AS.stack = stack}

updateStack :: MonadState AppState m => Stack Integer -> m ()
updateStack stack = do
  oldState <- get
  put $ oldState {AS.stack = stack}

pop :: (MonadState AppState m, MonadError ForthyError m) => m Integer
pop = do
  stack <- gets AS.stack
  case V.unsnoc $ ST.unStack stack of
    Nothing -> throwError DFTE.StackUnderflow
    Just (tl, hd) -> do
      updateStack $ ST.Stack tl
      pure hd