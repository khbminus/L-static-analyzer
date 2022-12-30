module Execute (run, run', execute, executeREPL) where

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

run :: [String] -> StateT Context IO ()
run strs = do
  parsed <- runMaybeT $ parse strs
  case parsed of
    Nothing  -> pure ()
    Just sts -> foldr ((>>) . execute) (pure ()) sts

run' :: Bool -> [String] -> StateT Context IO ()
run' optimize strs = do
  parsed <- runMaybeT $ parse strs
  case parsed of
    Nothing  -> pure ()
    Just sts -> foldr ((>>) . execute) (pure ()) (map optimizeLive sts ++ map (filter $ not . isFunctionDeclaration) sts)


execute :: [Statement] -> StateT Context IO ()
execute statements = do
  context <- get
  guard ( isNothing (Context.error context) )
  evaluateStatements statements

parse :: [String] -> MaybeT (StateT Context IO) [[Statement]]
parse (x:xs) = do
  case parseStatement x of
    Left err -> do { lift $ setErrorT $ ParserError err; mzero }
    Right parsed -> do
      parsedTail <- parse xs
      return $ parsed : parsedTail
parse [] = return []

executeREPL :: String -> StateT Context IO ()
executeREPL str = do
  context <- get
  guard ( isNothing (Context.error context) )
  case parseStatementOrExpression str of
    Left err -> setErrorT $ ParserError err
    Right (ConsoleStatement st) -> evaluateStatements st
    Right (ConsoleExpression ex) -> do
      res <- runMaybeT $ evaluateExpression ex
      case res of
        Nothing -> return ()
        Just val -> lift $ print val
