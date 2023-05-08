{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.List.Split (splitOn)
import Data.Text qualified as T
import Relude hiding (Op, Undefined, Word)
import Stack qualified as S
import Text.Regex.TDFA ((=~))

data Op = Add | Multiply deriving (Show, Eq)

data Eff = Print | Exit deriving (Show, Eq)

data Token = Effect Eff | Operator Op | Datum Integer | Blank deriving (Show, Eq)

tokenize :: Text -> Either Text Token
tokenize tokText
  | tokText =~ ("[0-9]+" :: Text) =
      case readMaybe $ T.unpack tokText of
        Nothing -> Left "Invalid integer input"
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
main = pure ()
