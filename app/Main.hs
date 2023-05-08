{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.List.Split (splitOn)
import Data.Text qualified as T
import Data.Vector qualified as V
import Relude hiding (Op, Undefined, Word)
import Stack (Stack (..))
import Stack qualified as S
import Text.Regex.TDFA ((=~))

data Op = Add | Multiply deriving (Show, Eq)

data Eff = Print | Exit deriving (Show, Eq)

data Token = Effect Eff | Operator Op | Datum Integer | Blank deriving (Show, Eq)

data ForthyError = StackUnderflow deriving (Show, Eq)

tokenize :: Text -> Either Text Token
tokenize tokText
  | tokText =~ ("[0-9]+" :: Text) =
      case readMaybe $ T.unpack tokText of
        Nothing -> Left "Invalid integer type"
        Just x -> Right $ Datum x
  | tokText == "+" = Right $ Operator Add
  | tokText == "*" = Right $ Operator Multiply
  | tokText == "." = Right $ Effect Print
  | tokText == "bye" = Right $ Effect Exit
  | tokText == " " = Right Blank
  | tokText == "" = Right Blank
  | tokText == "\n" = Right Blank
  | tokText == "\t" = Right Blank
  | otherwise = Left "Invalid token"

checkSize :: Int -> Stack -> Bool
checkSize requiredSize stack =
  length (S.unStack stack) >= requiredSize

add :: Stack -> Either ForthyError Stack
add stack =
  let size = 2
      (elems, stack') = V.splitAt size $ S.unStack stack
   in if checkSize size stack
        then Right $ S.push (sum elems) $ Stack stack'
        else Left StackUnderflow

multiply :: Stack -> Either ForthyError Stack
multiply stack =
  let size = 2
      (elems, stack') = V.splitAt size $ S.unStack stack
   in if checkSize size stack
        then Right $ S.push (product elems) $ Stack stack'
        else Left StackUnderflow

handleOps :: Op -> Stack -> Either ForthyError Stack
handleOps op stack =
  case op of
    Add -> add stack
    Multiply -> multiply stack

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

main :: IO ()
main = do
  let q = S.empty
      z = S.push 1 $ S.push 2 q
      z' = case handleOps Add z of
        Left err -> Left err
        Right s -> Right $ S.push 5 s

  print z'
  pure ()
