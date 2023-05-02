{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.HashMap.Strict qualified as HM
import Data.List.Split qualified as LS
import Data.Text qualified as T
import Relude hiding (Op, Word)
import Text.Regex.TDFA

data WordDatum = Int

data Word = Com Text | Op Text | Datum WordDatum

keywords :: HM.HashMap Text Word
keywords = HM.fromList [("print", Com "PRINT"), ("+", Op "ADD"), ("*", Op "MULTIPLY")]

tokenize :: Text -> [[Char]]
tokenize = LS.splitOn " " . T.unpack

main :: IO ()
main = do
  pure ()
