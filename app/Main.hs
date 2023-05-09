{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad.Error.Class (MonadError)
import Data.Default.Class (Default (def))
import Data.Stack (Stack)
import Data.Stack qualified as S
import Data.Text qualified as T
import Data.Types (AppState (..), Eff (..), ForthyError, Op (..), Token (..))
import Data.Types qualified as DT
import Data.Vector qualified as V
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA ((=~))

tokenize :: Text -> Either Text Token
tokenize tt
  | tt =~ ("[0-9]+" :: Text) =
      case readMaybe $ T.unpack tt of
        Nothing -> Left "Invalid integer type"
        Just x -> Right $ Datum x
  | tt == "+" = Right $ Operator Add
  | tt == "*" = Right $ Operator Multiply
  | tt == "." = Right $ Effect Print
  | tt == "bye" = Right $ Effect Exit
  | tt == " " = Right Blank
  | tt == "" = Right Blank
  | tt == "\n" = Right Blank
  | tt == "\t" = Right Blank
  | otherwise = Left "Invalid token"

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

handleOps :: (MonadState AppState m, MonadError ForthyError m) => Op -> m ()
handleOps =
  \case
    Add -> add
    Multiply -> multiply
    Dup -> dup

ePrint :: (MonadIO m, MonadState AppState m) => m ()
ePrint = gets DT.stack >>= print

handleEffs :: (MonadIO m, MonadState AppState m) => Eff -> m ()
handleEffs =
  \case
    Print -> ePrint
    Exit -> exitSuccess

exampleRun :: (MonadIO m, MonadState AppState m, MonadError ForthyError m) => m ()
exampleRun = do
  S.push 1
  S.push 2
  S.push 3
  ePrint
  add
  ePrint
  add
  ePrint
  _ <- S.pop
  ePrint

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

instance Default AppState where
  def :: AppState
  def = AppState {buffer = ["1", "2", "+"], stack = S.empty, isInCompileMode = False}

main :: IO ()
main = do
  runExceptStateT (def :: AppState) exampleRun >>= print
  pure ()
