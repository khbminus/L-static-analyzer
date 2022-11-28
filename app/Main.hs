module Main where

import Options.Applicative
import Grammar (parseInput)
import Console (runLoop, readEvalWriteLoop)
import ConsoleParser (Action(..), Input(..), actionParser, getInput, getVarContext)
import Context (Context(vars), emptyContext)

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
  let context = emptyContext { Context.vars = getVarContext varContext}
  runLoop context (lines i)

-- выход: q
runAction (Action Interactive varContext) =
  let context = emptyContext { Context.vars = getVarContext varContext} in
  readEvalWriteLoop context
