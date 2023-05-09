{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad.Error.Class (MonadError)
import Data.HashMap.Strict qualified as HM
import Data.List.Split qualified as DLS
import Data.Stack qualified as S
import Data.Text qualified as T
import Data.Types (AppState, Eff, ForthyError, Op, Token)
import Data.Types qualified as DT
import Relude hiding (Op, Undefined, Word, first, second, swap)
import System.Environment qualified as SE
import Text.Regex.TDFA ((=~))

tokenize :: Text -> Token
tokenize tt =
  if tt =~ ("[0-9]+" :: Text)
    then maybe DT.Blank DT.Datum (readMaybe $ T.unpack tt)
    else case tt of
      "+" -> DT.Operator DT.Add
      "*" -> DT.Operator DT.Multiply
      "dup" -> DT.Operator DT.Dup
      "drop" -> DT.Operator DT.Drop
      "swap" -> DT.Operator DT.Swap
      "over" -> DT.Operator DT.Over
      "rot" -> DT.Operator DT.Rot
      "." -> DT.Effect DT.Print
      "bye" -> DT.Effect DT.Exit
      "fun" -> DT.Operator DT.Fun
      "end" -> DT.Operator DT.End
      " " -> DT.Blank
      "" -> DT.Blank
      "\n" -> DT.Blank
      "\t" -> DT.Blank
      _rest -> DT.Identifier _rest

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
    DT.Drop -> void S.pop
    DT.Swap -> swap
    DT.Over -> over
    DT.Rot -> rot

ePrint :: (MonadState AppState m, MonadIO m, MonadError ForthyError m) => m ()
ePrint = do
  x <- S.pop
  putText $ show x <> " "

handleEffs :: (MonadIO m, MonadState AppState m, MonadError ForthyError m) => Eff -> m ()
handleEffs =
  \case
    DT.Print -> ePrint
    DT.Exit -> putTextLn "bye!" >> exitSuccess

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
  args <- SE.getArgs
  case args of
    [] -> error "Missing filename argument! Example: forthy main.fth"
    filename : _ -> do
      rawSource <- readFileBS filename
      case decodeUtf8' rawSource of
        Left err -> throwIO err
        Right source -> do
          t <- runExceptStateT appState eval
          putTextLn $ "\n\n<<DEBUG VIEW>>\n" <> show t
          where
            appState =
              DT.AppState
                { DT.buffer = splitText source,
                  DT.stack = S.empty,
                  DT.dictionary = HM.empty,
                  DT.isInCompileMode = False
                }