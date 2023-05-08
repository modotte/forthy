{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA ((=~))
import Data.Stack (Stack)
import Data.Stack qualified as S
import Data.Types

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

checkSize :: Int -> Stack -> Bool
checkSize requiredSize stack =
  length (S.unStack stack) >= requiredSize

add :: Stack -> Either ForthyError Stack
add stack =
  let size = 2
      (elems, stack') = V.splitAt size $ S.unStack stack
   in if checkSize size stack
        then Right $ S.push (sum elems) $ S.Stack stack'
        else Left StackUnderflow

multiply :: Stack -> Either ForthyError Stack
multiply stack =
  let size = 2
      (elems, stack') = V.splitAt size $ S.unStack stack
   in if checkSize size stack
        then Right $ S.push (product elems) $ S.Stack stack'
        else Left StackUnderflow

dup :: Stack -> Either ForthyError Stack
dup stack =
  case S.pop stack of
    Nothing -> Left StackUnderflow
    Just v -> Left StackUnderflow

handleOps :: Op -> Stack -> Either ForthyError Stack
handleOps op stack =
  case op of
    Add -> add stack
    Multiply -> multiply stack
    Dup -> dup stack

main :: IO ()
main = do
  let q = S.empty
      z = S.push 1 $ S.push 2 q
      z' = case handleOps Add z of
        Left err -> Left err
        Right s -> Right $ S.push 5 s

  print z'
  pure ()
