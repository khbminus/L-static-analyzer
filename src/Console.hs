module Console where

import Context (Context(..))
import Execute (run', executeREPL, applyToCode)
import System.IO ( hFlush, stdout )
import Control.Monad ( when )
import Control.Monad.State ( MonadTrans(lift), StateT (StateT) )
import Control.Monad.Trans.State ( StateT, get, put )
import Data.List (intercalate)
import Statement (Statement)
import Control.Monad (liftM)
import Compiler.Hoopl
import Analysis.AstToIr (astToIR)
import qualified Analysis.IR (Proc(..))

readEvalWriteLoop :: StateT Context IO ()
readEvalWriteLoop = do
    input <- lift $ prompt "L: "
    when (input /= "q") $ executeREPL input >> unsetError >> readEvalWriteLoop

runLoop :: Bool -> String -> StateT Context IO ()
runLoop live input = do
    run' live input
    context <- get
    maybe (pure ()) (lift . print) (Context.error context)

printExtended :: Bool -> String -> StateT Context IO ()
printExtended live input = do
    extended <- applyToCode live input f ""
    ctx <- get
    (lift . putStrLn) $ maybe extended show (Context.error ctx)
    where
        f sts = intercalate ";\n" (map show sts)

printIr :: Bool -> String -> StateT Context IO ()
printIr live input = do
    ir <- applyToCode live input f ""
    ctx <- get
    (lift . putStrLn) $ maybe ir show (Context.error ctx)
    where
        getIr sts = snd $ runSimpleUniqueMonad $ runWithFuel infiniteFuel (astToIR sts)

        prettyShow p = "IR for function " ++ Analysis.IR.name p ++ ":\n" ++ showGraph show (Analysis.IR.body p)

        f sts = intercalate "\n\n" (map prettyShow $ getIr sts)

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
