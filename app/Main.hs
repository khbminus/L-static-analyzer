module Main where

import Options.Applicative
import CommandLineParser (Action(..), actionParser, getInput, getVarContext, runInterpreter)
import Grammar (parseInput)

-- Это консольное приложение, которое умеет запускать один из двух парсеров на строке,
-- прочитанной из файла или переданной в качестве аргумента командной строки.
-- Тут используется пакет optparse-applicative, позволяющий задавать аргументы командной строки,
-- а также генерирует help и проводит обработку пользовательских ошибок.

-- Для запуска приложения пишем stack run exe, дальше два минуса (--), после которых идут уже
-- аргументы командной строки нашего приложения.
-- Например, stack run exe -- --help выводит описание приложения и допустимых аргументов.

-- optparse-applicative позволяет задать парсеры для отдельных аргументов командной строки,
-- а потом объединить их, используя (<*>) (см. actionParser)
-- при этом порядок аргументов командной строки будет произвольным.

-- Каждый аргумент командной строки сопровождается модификаторами опций Mod.
-- Mod является моноидом, поэтому разные модификаторы можно объединять, используя (<>)
-- Некоторые модификаторы:
-- * short, принимающий значение типа Char, описывает короткое название аргумента
-- * long, принимающий String, описывает длинное название аргумента
-- * help, принимающий String, задает поясняющий аргумент текст
-- * value задает значение по умолчанию для аргумента
-- * metavar задает метапеременную, используемую в документации для значения аргумента

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

-- Основная функция приложения
runAction :: Action -> IO ()
runAction (Action input context) = do
  i <- getInput input         -- подготавливаем входные данные
  let sts = parseInput i
  let varContext = getVarContext context
  runInterpreter varContext sts

