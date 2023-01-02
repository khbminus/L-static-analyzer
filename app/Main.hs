module Main where

import Options.Applicative ( (<**>), fullDesc, header, info, progDesc, execParser, helper )
import Console (runLoop, readEvalWriteLoop, printIr, printExtended)
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
runAction (Action (FileInput path) live varContext extend ir) = do
  i <- readFile path
  let context = newContext { Context.vars = [getVarContext varContext]}
  case (ir, extend) of
    (False, False) -> evalStateT (runLoop live i) context
    (False, True) -> evalStateT (printExtended live i) context
    (True, False) -> evalStateT (printIr live i) context
    (True, True) -> evalStateT (do {printExtended live i; printIr live i}) context

-- выход: q
runAction (Action Interactive _ varContext extend ir) =
  if extend || ir then error "can't handle extend or IR option in REPL mode" else
  let context = newContext { Context.vars = [getVarContext varContext]} in
  evalStateT readEvalWriteLoop context
