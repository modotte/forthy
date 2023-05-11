{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad.Error.Class (MonadError, throwError)
import Data.AppState (AppState)
import Data.AppState qualified as AS
import Data.Forthy.Effect qualified as DFE
import Data.Forthy.Operator qualified as DFO
import Data.Forthy.Types.Effect qualified as DFTEFF
import Data.Forthy.Types.Error (ForthyError)
import Data.Forthy.Types.Error qualified as DFTE
import Data.Forthy.Types.Operator qualified as DFTO
import Data.Forthy.Types.Token
import Data.Forthy.Types.Token qualified as DFTT
import Data.HashMap.Strict qualified as HM
import Data.List.Split qualified as DLS
import Data.Stack qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Relude hiding (Op, Undefined, Word, first, second, swap)
import System.Environment qualified as SE
import Text.Regex.TDFA ((=~))

tokenize :: Text -> Token
tokenize tt =
  if tt =~ ("^[0-9]+$" :: Text)
    then maybe DFTT.Blank DFTT.Datum (readMaybe $ T.unpack tt)
    else case tt of
      "+" -> DFTT.Operator DFTO.Add
      "*" -> DFTT.Operator DFTO.Multiply
      "dup" -> DFTT.Operator DFTO.Dup
      "drop" -> DFTT.Operator DFTO.Drop
      "swap" -> DFTT.Operator DFTO.Swap
      "over" -> DFTT.Operator DFTO.Over
      "rot" -> DFTT.Operator DFTO.Rot
      "emit" -> DFTT.Effect DFTEFF.Emit
      "cr" -> DFTT.Effect DFTEFF.Newline
      "=" -> DFTT.Operator DFTO.Equal
      "invert" -> DFTT.Operator DFTO.Invert
      "or" -> DFTT.Operator DFTO.Or
      "and" -> DFTT.Operator DFTO.And
      ">" -> DFTT.Operator DFTO.LargerThan
      "<" -> DFTT.Operator DFTO.SmallerThan
      "." -> DFTT.Effect DFTEFF.Print
      "bye" -> DFTT.Effect DFTEFF.Exit
      "fun" -> DFTT.Operator DFTO.Fun
      "endfun" -> DFTT.Operator DFTO.EndFun
      _rest -> DFTT.Identifier _rest

evalEnv :: (MonadState AppState m, MonadIO m, MonadError ForthyError m) => Token -> m ()
evalEnv token = do
  s <- get
  let dict = AS.dictionary s
      compileMode = AS.isInCompileMode s
      cidx = AS.currentCompileIdentifier s

  if compileMode
    then case cidx of
      Nothing ->
        case token of
          DFTT.Identifier idx ->
            put $
              s
                { AS.dictionary = HM.insert idx V.empty dict,
                  AS.currentCompileIdentifier = Just idx
                }
          _ -> pure ()
      Just idx -> do
        if token == DFTT.Operator DFTO.EndFun
          then DFO.handleOps DFTO.EndFun
          else put $ s {AS.dictionary = HM.update (\xs -> Just $ V.snoc xs token) idx dict}
    else case token of
      DFTT.Datum x -> S.push x
      DFTT.Effect eff -> DFE.handleEffs eff
      DFTT.Operator op -> DFO.handleOps op
      DFTT.Blank -> pure ()
      DFTT.Identifier idx -> do
        case HM.lookup idx dict of
          Nothing -> throwError $ DFTE.MissingIdentifier idx
          Just tokens ->
            foldr
              (\x acc -> evalEnv x >>= pure acc)
              (pure ())
              tokens

eval :: (MonadIO m, MonadState AppState m, MonadError ForthyError m) => m ()
eval = do
  buffer <- gets AS.buffer
  let tokens = tokenize <$> buffer
  foldr
    (\x acc -> evalEnv x >>= pure acc)
    (pure ())
    tokens

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

splitText :: Text -> [Text]
splitText source =
  filter (/= "") splitted
  where
    splitted = T.pack <$> DLS.splitOneOf " \n\t" text
    text = T.unpack source

main :: IO ()
main = do
  args <- SE.getArgs
  case args of
    [] -> error "Missing filename argument! Example: forthy main.fth"
    filename : _ -> do
      rawSource <- readFileBS filename
      case decodeUtf8' rawSource of
        Left err -> throwIO err
        Right source -> do
          t <- runExceptStateT appState eval
          putTextLn $ "\n\n<<DEBUG VIEW>>\n" <> show t
          print $ splitText source
          where
            appState =
              AS.AppState
                { AS.buffer = splitText source,
                  AS.stack = S.empty,
                  AS.dictionary = HM.empty,
                  AS.isInCompileMode = False,
                  AS.currentCompileIdentifier = Nothing
                }