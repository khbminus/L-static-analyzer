module Main where

import Options.Applicative
import Console (runLoop, readEvalWriteLoop)
import ConsoleParser (Action(..), Input(..), actionParser, getInput, getVarContext)
import Context (Context(vars), newContext)
import Control.Monad.State

-- Программа парсит аргументы командной строки при помощи execParser,
-- а потом запускает функцию runAction (логику приложения)
main :: IO ()
main = do
    runAction =<< execParser opts
  where
    -- Задает парсер аргументов actionParser, сопровождая его автоматической генерацией странички help.
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "This application executes programms in L"
      <> header "L interpreter"
      )

runAction :: Action -> IO ()
runAction (Action input@(FileInput _) varContext) = do
  i <- getInput input
  let context = newContext { Context.vars = getVarContext varContext}
  evalStateT (runLoop $ lines i) context

-- выход: q
runAction (Action Interactive varContext) =
  let context = newContext { Context.vars = getVarContext varContext} in
  evalStateT readEvalWriteLoop context
