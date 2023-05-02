{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.HashMap.Strict qualified as HM
import Data.List.Split qualified as LS
import Data.Text qualified as T
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA

data WordDatum = Int

data Word = Com Text | Op Text | Datum WordDatum | Undefined

words :: HM.HashMap Text Word
words = HM.fromList [("print", Com "PRINT"), ("+", Op "ADD"), ("*", Op "MULTIPLY")]

tokenize :: Text -> [[Char]]
tokenize = LS.splitOn " " . T.unpack

matchToken :: (Eq a, IsString a) => a -> Word
matchToken "print" = Com "PRINT"
matchToken "+" = Op "ADD"
matchToken "*" = Op "MULTIPLY"
matchToken _ = Undefined

main :: IO ()
main = do
  pure ()
