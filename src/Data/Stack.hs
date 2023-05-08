module Data.Stack (module Data.Stack.Types, empty, push, pop) where

import Data.Stack.Types
import Data.Stack.Types qualified as ST
import Data.Types (AppState, ForthyError (..))
import Data.Types qualified as DT
import Data.Vector qualified as V
import Relude hiding (empty, state)
import Relude.Extra qualified as RE

empty :: Stack
empty = ST.Stack V.empty

addStack :: Integer -> Stack -> Stack
addStack x = RE.under $ V.cons x

push :: (MonadState AppState m) => Integer -> m ()
push x =
  modify' $ \state ->
    let stack = addStack x $ DT.stack state
     in state {DT.stack = stack}

checkSize :: Int -> Stack -> Bool
checkSize requiredSize stack =
  length (ST.unStack stack) >= requiredSize

updateStack :: (MonadState AppState m) => Stack -> m ()
updateStack stack = do
  oldState <- get
  put $ oldState {DT.stack = stack}

pop :: (MonadState AppState m) => m (Either ForthyError Integer)
pop = do
  stack <- gets DT.stack
  if checkSize 1 stack
    then do
      let (hd, tl) = V.splitAt 1 $ ST.unStack stack
      updateStack $ ST.Stack tl
      pure $ Right $ V.head hd
    else pure $ Left StackUnderflow