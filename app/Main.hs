module Main where

import Options.Applicative ( (<**>), fullDesc, header, info, progDesc, execParser, helper )
import Console (runLoop, readEvalWriteLoop)
import ConsoleParser (Action(..), Input(..), actionParser, getVarContext)
import Context (Context(vars), newContext)
import Control.Monad.State ( evalStateT )

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
runAction (Action (FileInput path) live varContext) = do
  i <- readFile path
  let context = newContext { Context.vars = [getVarContext varContext]}
  evalStateT (runLoop live $ lines i) context

-- выход: q
runAction (Action Interactive _ varContext) =
  let context = newContext { Context.vars = [getVarContext varContext]} in
  evalStateT readEvalWriteLoop context
