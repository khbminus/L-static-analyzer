module Console where

import Context (Context(..))
import Execute (execute, run)
import System.IO ( hFlush, stdout )
import Control.Monad (when)
import Control.Monad.State ( MonadTrans(lift) )
import Control.Monad.Trans.State ( StateT, get, put )

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
