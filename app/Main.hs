{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.HashMap.Strict qualified as HM
import Data.List.Split qualified as LS
import Data.Text qualified as T
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA

data Word = Com Text | Op Text | Datum Int | Undefined deriving (Show, Eq)

words :: HM.HashMap Text Word
words = HM.fromList [("print", Com "PRINT"), ("+", Op "ADD"), ("*", Op "MULTIPLY")]

tokenize :: Text -> [[Char]]
tokenize = LS.splitOn " " . T.unpack

parseToken :: String -> Word
parseToken token
  | token =~ ("[0-9]+" :: String) =
      maybe Undefined Datum $ readMaybe token
  | token == "+" = Op "ADD"
  | token == "*" = Op "MULTIPLY"
  | token == "print" = Com "PRINT"
  | otherwise = Undefined

main :: IO ()
main = do
  print $ parseToken <$> tokenize "1 2 + print"
