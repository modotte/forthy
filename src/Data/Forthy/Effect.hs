{-# LANGUAGE LambdaCase #-}

module Data.Forthy.Effect (module Data.Forthy.Types.Effect, handleEffs) where

import ASCII qualified
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Forthy.Types.Effect
import Data.Forthy.Types.Error (ForthyError)
import Data.Forthy.Types.Error qualified as DFTE
import Data.Stack qualified as S
import Data.Types (AppState)
import Relude

ePrint :: (MonadState AppState m, MonadIO m, MonadError ForthyError m) => m ()
ePrint = do
  x <- S.pop
  putText $ show x <> " "

emit :: (MonadIO m, MonadState AppState m, MonadError ForthyError m) => m ()
emit = do
  x <- S.pop
  case ASCII.intToCharMaybe $ fromIntegral x of
    Nothing -> throwError $ DFTE.InvalidASCIIValue x
    Just y -> putText $ ASCII.charListToText [y]

handleEffs :: (MonadIO m, MonadState AppState m, MonadError ForthyError m) => Eff -> m ()
handleEffs =
  \case
    Print -> ePrint
    Exit -> putTextLn "bye!" >> exitSuccess
    Emit -> emit
    Newline -> putTextLn ""