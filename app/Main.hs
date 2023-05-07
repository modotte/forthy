{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.HashMap.Strict qualified as HM
import Data.List.Split qualified as LS
import Data.Text qualified as T
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA

data Word = Com Text | Op Text | Datum Int | Undefined deriving (Show, Eq)

tokenize :: Text -> [Text]
tokenize =
  fmap T.pack <$> sp
  where
    sp = LS.splitOn " " . T.unpack

parseToken :: Text -> Word
parseToken token
  | token =~ ("[0-9]+" :: Text) =
      maybe Undefined Datum $ readMaybe $ T.unpack token
  | token == "+" = Op "ADD"
  | token == "*" = Op "MULTIPLY"
  | token == "." = Com "PRINT"
  | otherwise = Undefined

execStack = []

main :: IO ()
main = do
  print $ parseToken <$> tokenize "1 2 + ."
