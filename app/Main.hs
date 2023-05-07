{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.HashMap.Strict qualified as HM
import Data.List.Split qualified as LS
import Data.Text qualified as T
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA

data Op = Add | Multiply deriving (Show, Eq)

data Comm = Print | Exit deriving (Show, Eq)

data Token = Command Comm | Operator Op | Datum Int deriving (Show, Eq)

tokenize :: Text -> [Text]
tokenize =
  fmap T.pack <$> xs
  where
    xs = LS.splitOn " " . T.unpack

parse :: Text -> Either Text Token
parse token
  | token =~ ("[0-9]+" :: Text) =
      case readMaybe $ T.unpack token of
        Nothing -> Left "Invalid integer input"
        Just x -> Right $ Datum x
  | token == "+" = Right $ Operator Add
  | token == "*" = Right $ Operator Multiply
  | token == "." = Right $ Command Print
  | token == "bye" = Right $ Command Exit
  | otherwise = Left "Invalid token"

type ExecStack = [Int]

handleOps :: Op -> ExecStack -> ExecStack
handleOps op execStack = case op of
  Add ->
    case execStack of
      [] -> []
      x : xs ->
        case xs of
          [] -> []
          y : zs -> (x + y) : zs
  Multiply ->
    case execStack of
      [] -> []
      x : xs ->
        case xs of
          [] -> []
          y : zs -> x * y : zs

handleComms :: Comm -> ExecStack -> IO ()
handleComms comm execStack = do
  case comm of
    Print -> print execStack
    Exit -> exitSuccess

type Term = (Token, ExecStack)

eval :: Token -> ExecStack -> IO (Either Text Term)
eval token execStack = case token of
  Datum x -> pure $ Right (token, x : execStack)
  Operator x -> pure $ Right (token, handleOps x execStack)
  Command x -> do
    _ <- handleComms x execStack
    pure $ Right (token, execStack)

read :: IO Text
read =
  putStr "$> "
    >> hFlush stdout
    >> getLine

main :: IO ()
main = do
  pure ()
