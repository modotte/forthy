{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.List.Split (splitOn)
import Data.Text qualified as T
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA ((=~))

data Op = Add | Multiply deriving (Show, Eq)

data Comm = Print | Exit deriving (Show, Eq)

data Token = Command Comm | Operator Op | Datum Int | Blank deriving (Show, Eq)

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
  | tokText == " " = Right Blank
  | tokText == "" = Right Blank -- TODO: Isolate for REPL only.
  | tokText == "\n" = Right Blank
  | tokText == "\t" = Right Blank
  | otherwise = Left "Invalid token"

splitted :: Text -> [Text]
splitted source = T.pack <$> splitOn " " (T.unpack source)

tok :: Text -> [Either Text Token]
tok source =
  filter
    ( \case
        Left _ -> True
        Right x -> x /= Blank
    )
    $ map tokenize (splitted source)

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
    Blank -> Right (token, execStack)
    _ -> Left "Command not handled yet"

read :: IO Text
read =
  putStr "$> "
    >> hFlush stdout
    >> getLine

exec :: Text -> [Int] -> Either Text Term
exec input stackExec = do
  case tokenize input of
    Left err -> Left err
    Right token ->
      case eval (token, stackExec) of
        Left err -> Left err
        Right term -> Right term

main :: IO ()
main = do
  input <- read
  case exec input [] of
    Left err -> do
      print err
      main
    Right _ -> main
