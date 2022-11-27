module Main where

import Options.Applicative
import CommandLineParser (Action(..), actionParser, getInput, getVarContext, runInterpreter, Input(..))
import Grammar (parseInput)
import Context (emptyContext, Context(..))
import Execute (execute)
import Control.Monad (when)
import System.IO

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
runAction (Action input@(FileInput _) context) = do
  i <- getInput input
  let parsed = parseInput i
  case parsed of
    Left err -> print err
    Right sts -> let varContext = getVarContext context in
      runInterpreter varContext sts

-- выход: q
-- TODO: STYLE FIX!!!
runAction (Action Interactive context) = interpret $ emptyContext { Context.vars = getVarContext context }
  where
    interpret :: Context -> IO ()
    interpret cxt = do
      line <- prompt "L: "
      when (line /= "q") $
        let parsed = parseInput line in
        case parsed of
          Left err -> print err >> interpret cxt
          Right sts -> do
            newcxt <- execute cxt sts
            case newcxt of
              Context { Context.error = Just err } -> do
                print err 
                interpret $ newcxt { Context.error = Nothing}
              _ -> do
                interpret newcxt

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine