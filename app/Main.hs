{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Monad.Error.Class (MonadError)
import Data.Stack (Stack)
import Data.Stack qualified as S
import Data.Text qualified as T
import Data.Types
import Data.Vector qualified as V
import Relude hiding (Op, Undefined, Word)
import Text.Regex.TDFA ((=~))

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

add :: (MonadState AppState m, MonadError ForthyError m) => m ()
add = do
  x <- S.pop
  y <- S.pop
  S.push $ x + y

multiply :: (MonadState AppState m, MonadError ForthyError m) => m ()
multiply = do
  x <- S.pop
  y <- S.pop
  S.push $ x * y

main :: IO ()
main = do
  pure ()
