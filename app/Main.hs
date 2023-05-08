{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Monad.Error.Class (MonadError)
import Data.Default.Class (Default (def))
import Data.Stack (Stack)
import Data.Stack qualified as S
import Data.Text qualified as T
import Data.Types
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

handleEffs :: (MonadIO m, MonadState AppState m, MonadError ForthyError m) => Eff -> m ()
handleEffs =
  \case
    Print -> gets DT.stack >>= print
    Exit -> exitSuccess

runStateExceptT :: Monad m => s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

instance Default AppState where
  def :: AppState
  def = AppState {buffer = [], stack = S.empty}

main :: IO ()
main = do
  pure ()
