module Console where

import Context (Context(..), setErrorT)
import Execute (run, execute)
import System.IO ( hFlush, stdout )
import Control.Monad ( when, guard )
import Control.Monad.State ( MonadTrans(lift) )
import Control.Monad.Trans.State ( StateT, get, put )
import Evaluate (evaluateStatements, evaluateExpression)
import ConsoleParser (REPLInput(..), parseStatementOrExpression)
import Error (RuntimeError(ParserError))
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Maybe (isJust, fromJust, isNothing)


executeREPL :: String -> StateT Context IO ()
executeREPL str = do
    context <- get
    guard (isNothing (Context.error context))
    case parseStatementOrExpression str of
        Left err -> setErrorT $ ParserError err
        Right (CStatement st) -> evaluateStatements st
        Right (CExpression ex) -> do
            res <- runMaybeT $ evaluateExpression ex
            guard (isJust res)
            lift $ print (fromJust res)

-- TODO: print expression results
readEvalWriteLoop :: StateT Context IO ()
readEvalWriteLoop = do
    input <- lift $ prompt "L: "
    when (input /= "q") $ execute input >> unsetError >> readEvalWriteLoop

runLoop :: [String] -> StateT Context IO ()
runLoop input = do
    run input
    context <- get
    maybe (pure ()) (lift . print) (Context.error context)

unsetError :: StateT Context IO ()
unsetError = do
    context <- get
    let f err = lift (print err) >> put (context { Context.error = Nothing })
    maybe (pure ()) f (Context.error context)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine
