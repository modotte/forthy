{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.HashMap.Strict qualified as HM
import Relude hiding (Op, Word)

data WordDatum = Int

data Word = Com Text | Op Text | Datum WordDatum

keywords :: HM.HashMap Text Word
keywords = HM.fromList [("print", Com "PRINT"), ("+", Op "ADD"), ("*", Op "MULTIPLY")]

main :: IO ()
main = do
  pure ()
