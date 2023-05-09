{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad.Error.Class (MonadError)
import Data.List.Split qualified as DLS
import Data.Stack qualified as S
import Data.Text qualified as T
import Data.Types (AppState, Eff, ForthyError, Op, Token)
import Data.Types qualified as DT
import Relude hiding (Op, Undefined, Word, first, second, swap)
import Text.Regex.TDFA ((=~))

tokenize :: Text -> Token
tokenize tt
  | tt =~ ("[0-9]+" :: Text) =
      maybe DT.Blank DT.Datum (readMaybe $ T.unpack tt)
  | tt == "+" = DT.Operator DT.Add
  | tt == "*" = DT.Operator DT.Multiply
  | tt == "dup" = DT.Operator DT.Dup
  | tt == "drop" = DT.Operator DT.Drop
  | tt == "swap" = DT.Operator DT.Swap
  | tt == "over" = DT.Operator DT.Over
  | tt == "rot" = DT.Operator DT.Rot
  | tt == "." = DT.Effect DT.Print
  | tt == "bye" = DT.Effect DT.Exit
  | tt == " " = DT.Blank
  | tt == "" = DT.Blank
  | tt == "\n" = DT.Blank
  | tt == "\t" = DT.Blank
  | otherwise = DT.Blank

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

handleOps :: (MonadState AppState m, MonadError ForthyError m) => Op -> m ()
handleOps =
  \case
    DT.Add -> add
    DT.Multiply -> multiply
    DT.Dup -> dup
    DT.Drop -> do
      _ <- S.pop
      pure ()
    DT.Swap -> swap
    DT.Over -> over
    DT.Rot -> rot

ePrint :: (MonadState AppState m, MonadIO m, MonadError ForthyError m) => m ()
ePrint = do
  x <- S.pop
  print x

handleEffs :: (MonadIO m, MonadState AppState m, MonadError ForthyError m) => Eff -> m ()
handleEffs =
  \case
    DT.Print -> ePrint
    DT.Exit -> do
      putTextLn "bye!"
      exitSuccess

evalEnv :: (MonadState AppState m, MonadIO m, MonadError ForthyError m) => Token -> m ()
evalEnv token =
  case token of
    DT.Datum x -> S.push x
    DT.Effect eff -> handleEffs eff
    DT.Operator op -> handleOps op
    DT.Blank -> pure ()

eval :: (MonadIO m, MonadState AppState m, MonadError ForthyError m) => m ()
eval = do
  buffer <- gets DT.buffer
  let tokens = tokenize <$> buffer
  foldr
    (\x acc -> evalEnv x >>= pure acc)
    (pure ())
    tokens

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

splitText :: Text -> [Text]
splitText source = T.pack <$> DLS.splitOn " " (T.unpack source)

main :: IO ()
main = do
  let appState =
        DT.AppState
          { DT.buffer = splitText "1 dup + . 5 10 over 1 2 3 rot swap .",
            DT.stack = S.empty,
            DT.isInCompileMode = False
          }
  runExceptStateT appState eval >>= print
  pure ()