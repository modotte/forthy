{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.Text qualified as T
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA ((=~))

data Op = Add | Multiply deriving (Show, Eq)

data Comm = Print | Exit deriving (Show, Eq)

data Token = Command Comm | Operator Op | Datum Int deriving (Show, Eq)

tokenize :: Text -> Either Text Token
tokenize tokText
  | tokText =~ ("[0-9]+" :: Text) =
      case readMaybe $ T.unpack tokText of
        Nothing -> Left "Invalid integer input"
        Just x -> Right $ Datum x
  | tokText == "+" = Right $ Operator Add
  | tokText == "*" = Right $ Operator Multiply
  | tokText == "." = Right $ Command Print
  | tokText == "bye" = Right $ Command Exit
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

eval :: Term -> Either Text Term
eval (token, execStack) =
  case token of
    Datum x -> Right (token, x : execStack)
    Operator x -> Right (token, handleOps x execStack)
    _ -> Left "Command not handled yet"

read :: IO Text
read =
  putStr "$> "
    >> hFlush stdout
    >> getLine

exec :: Text -> Either Text Term
exec input = do
  case tokenize input of
    Left err -> Left err
    Right token ->
      case eval (token, []) of
        Left err -> Left err
        Right term -> Right term

main :: IO ()
main = do
  input <- read
  case exec input of
    Left err -> do
      print err
      main
    Right _ -> main
