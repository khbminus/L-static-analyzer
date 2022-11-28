{-# LANGUAGE PatternSynonyms #-}
module Console where

import Statement (Statement)
import Context (Context(..), pattern ErrorContext)
import Execute (run, execute)
import Grammar (parseInput)
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
import System.IO ( hFlush, stdout )
import Control.Monad (when)
import Control.Applicative ( Alternative(empty) )
import Error (RuntimeError)
import Control.Monad.Trans.Class ( MonadTrans(lift) )


runFromString :: Context -> String -> IO ()
runFromString context str = do
    parsed <- runMaybeT $ safeParseInput str
    case parsed of
        Nothing -> pure ()
        Just sts -> run context sts

execFromString :: Context -> String -> IO Context
execFromString context str = do
    parsed <- runMaybeT $ safeParseInput str
    maybe (pure context) (execute context) parsed

safeParseInput :: String -> MaybeT IO [Statement]
safeParseInput str = do
    case parseInput str of
        Left err -> lift (print err) >> empty
        Right sts -> return sts

-- TODO: print expression results
readEvalWriteLoop :: Context -> IO ()
readEvalWriteLoop context = do
    input <- prompt "L: "
    when (input /= "q") $ execFromString context input >>= unsetError >>= readEvalWriteLoop

runLoop :: Context -> [String] -> IO ()
runLoop c@ErrorContext _ = print $ Context.error c
runLoop c (x:xs) = execFromString c x >>= runLoop' xs where
    runLoop' sts cxt = runLoop cxt sts
runLoop _ [] = pure ()

unsetError :: Context -> IO Context
unsetError context = maybe (pure context) f (Context.error context) where
    f :: RuntimeError -> IO Context
    f err = print err >> pure (context { Context.error = Nothing })

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine
