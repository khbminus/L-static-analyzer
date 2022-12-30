module Execute (run, execute, executeREPL) where

import Context ( Context(..), setErrorT )
import Control.Monad.State
import Error (RuntimeError (..))
import Evaluate (evaluateStatements, evaluateExpression)
import Grammar (parseStatement, REPLInput (..), parseStatementOrExpression)
import Data.Maybe (isNothing)
import Control.Monad.Trans.Maybe (MaybeT(..))

run :: [String] -> StateT Context IO ()
run = foldr ((>>) . execute) (return ())

execute :: String -> StateT Context IO ()
execute str = do
  context <- get
  guard ( isNothing (Context.error context) )
  case parseStatement str of
    Left err -> setErrorT $ ParserError err
    Right statements -> evaluateStatements statements

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
