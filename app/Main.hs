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

data Token = Command Comm | Operator Op | Datum Int | Undefined deriving (Show, Eq)

tokenize :: Text -> [Text]
tokenize =
  fmap T.pack <$> sp
  where
    sp = LS.splitOn " " . T.unpack

parseToken :: Text -> Token
parseToken token
  | token =~ ("[0-9]+" :: Text) =
      maybe Undefined Datum $ readMaybe $ T.unpack token
  | token == "+" = Operator Add
  | token == "*" = Operator Multiply
  | token == "." = Command Print
  | token == "bye" = Command Exit
  | otherwise = Undefined

type ExecStack = [Int]

handleOps :: Op -> ExecStack -> (Int, ExecStack)
handleOps op execStack = case op of
  Add ->
    case execStack of
      [] -> (0, [])
      x : xs ->
        case xs of
          [] -> (0, [])
          y : zs -> (x + y, zs)
  Multiply ->
    case execStack of
      [] -> (0, [])
      x : xs ->
        case xs of
          [] -> (0, [])
          y : zs -> (x + y, zs)

eval :: Text -> ExecStack -> IO ExecStack
eval text execStack =
  pure $
    fmap
      ( \case
          Datum x -> x
          Operator x -> fst $ handleOps x execStack
          _ -> 0
      )
      tokens
  where
    tokens = parseToken <$> tokenize text

read :: IO Text
read =
  putStr "FORTHY> "
    >> hFlush stdout
    >> getLine

main :: IO ()
main = do
  input <- read

  case parseToken input of
    Command x ->
      case x of
        Exit -> pure ()
        _ -> pure ()
    _ -> eval input [] >> main
