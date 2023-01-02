module Execute (run, run', execute, executeREPL, applyToCode) where

import Context ( Context(..), setErrorT )
import Control.Monad.State
import Error (RuntimeError (..))
import Evaluate (evaluateStatements, evaluateExpression)
import Grammar (parseStatement, REPLInput (..), parseStatementOrExpression)
import Data.Maybe (isNothing)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Statement (Statement)
import Analysis.Live (optimizeLive)
import Analysis.AstToIr (isFunctionDeclaration)
import Text.Megaparsec (errorBundlePretty)

run :: String -> StateT Context IO ()
run strs = do
  parsed <- runMaybeT $ parse strs
  case parsed of
    Nothing  -> pure ()
    Just sts -> execute sts

run' :: Bool -> String -> StateT Context IO ()
run' optimize strs = do
  parsed <- runMaybeT $ parse strs
  case parsed of
    Nothing  -> pure ()
    Just sts -> if optimize then execute (optimizeLive sts ++ filter (not . isFunctionDeclaration) sts) else execute $ sts

applyToCode :: Bool -> String -> ([Statement] -> a) -> a -> StateT Context IO a
applyToCode optimize strs f defaultValue = do
  parsed <- runMaybeT $ parse strs
  case parsed of
    Nothing -> return defaultValue
    Just sts -> do
      let realSts = if optimize then optimizeLive sts ++ filter (not . isFunctionDeclaration) sts else sts
      return $ f realSts


execute :: [Statement] -> StateT Context IO ()
execute statements = do
  context <- get
  guard ( isNothing (Context.error context) )
  evaluateStatements statements

parse :: String -> MaybeT (StateT Context IO) [Statement]
parse x = do
  case parseStatement x of
    Left err -> do { lift $ setErrorT $ ParserError $ errorBundlePretty err; mzero }
    Right parsed ->
      return parsed
executeREPL :: String -> StateT Context IO ()
executeREPL str = do
  context <- get
  guard ( isNothing (Context.error context) )
  case parseStatementOrExpression str of
    Left err -> setErrorT $ ParserError $ errorBundlePretty err
    Right (ConsoleStatement st) -> evaluateStatements st
    Right (ConsoleExpression ex) -> do
      res <- runMaybeT $ evaluateExpression ex
      case res of
        Nothing -> return ()
        Just val -> lift $ print val
