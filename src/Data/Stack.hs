module Data.Stack (module Data.Stack.Types, empty, push, pop) where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.Stack.Types
import Data.Stack.Types qualified as ST
import Data.Types (AppState, ForthyError (..))
import Data.Types qualified as DT
import Data.Vector (Vector)
import Data.Vector qualified as V
import Relude hiding (empty, print, state)

empty :: Stack
empty = ST.Stack V.empty

addStack :: Integer -> Stack -> Stack
addStack x (ST.Stack s) = ST.Stack $ V.snoc s x

push :: (MonadState AppState m) => Integer -> m ()
push x =
  modify' $ \state ->
    let stack = addStack x $ DT.stack state
     in state {DT.stack = stack}

updateStack :: (MonadState AppState m) => Stack -> m ()
updateStack stack = do
  oldState <- get
  put $ oldState {DT.stack = stack}

pop :: (MonadState AppState m, MonadError ForthyError m) => m Integer
pop = do
  stack <- gets DT.stack
  case V.unsnoc $ ST.unStack stack of
    Nothing -> throwError StackUnderflow
    Just (tl, hd) -> do
      updateStack $ ST.Stack tl
      pure hd
