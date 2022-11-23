module Main (main) where

import Options.Applicative
import System.FilePath ((<.>))
import Text.Printf (printf)

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

-- Некоторые виды аргументов командной строки:
-- * flag -- не имеет непосредственно аргумента (как -r в rm -r).
--   Функция flag принимает первым аргументом значение по умолчанию, которое возвращается, если флаг не установлен,
--   а вторым аргументом -- значение, которое устанавливается, если флаг установлен.
--   Пример использования можно увидеть в parserTypeParser.
-- * switch == flag False True. Пример можно увидеть в dumpToStdoutParser.
-- * strOption -- строковый аргумент командной строки (как -o Main в ghc Main.hs -o Main)
-- * option -- аргумент, который можно прочитать при помощи read из класса Read
-- * strArgument -- строковый аргумент командной строки без отдельного флага (как Main.hs в ghc Main.hs -o Main)
-- * argument -- аргумент командной строки без отдельного флага, который можно прочитать при помощи read.

-- Программа парсит аргументы командной строки при помощи execParser,
-- а потом запускает функцию runAction (логику приложения)
main :: IO ()
main = do
    runAction =<< execParser opts
  where
    -- Задает парсер аргументов actionParser, сопровождая его автоматической генерацией странички help.
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "This application echoes input file."
      <> header "Simple echo console application"
      )

-- Тип данных, агрегирующий все аргументы командной строки, возвращается actionParser-ом
data Action = Action
            { input :: Input
            , output :: Output
            , dumpToStdout :: Bool }
            deriving (Show)

-- Парсер аргументов командной строки
actionParser :: Parser Action
actionParser =
  Action <$> inputParser <*> outputParser <*> dumpToStdoutParser

-- Основная функция приложения
runAction :: Action -> IO ()
runAction (Action input output dump) = do
  i <- getInput input         -- подготавливаем входные данные
  o <- getOutput output input -- подготавливаем файл для результат
  runEcho i o
  dumpIntoStdout dump i o     -- если стоит соответствующий флаг, выводим результат работы приложения в консоль

-- Тип входных данных
data Input = FileInput FilePath -- Имя входного файла
           | StrInput String    -- Строка, передаваемая аргументом командной строки
           deriving (Show)

-- Тип выходных данных
data Output = FileOutput FilePath -- Имя файла для результата
            | DefaultOutput       -- Дефолтное имя файла
            deriving (Show)

-- Парсер аргумента, специфицирующий, откуда брать входные данные
inputParser :: Parser Input
inputParser = fileInput <|> strInput

-- Флаг -i/--input позволяет задать строку -- имя входного файла
fileInput :: Parser Input
fileInput = FileInput <$> strOption --
  (  short 'i'           -- короткое имя флага (-i)
  <> long "input"        -- длинное имя флага (--input)
  <> metavar "INPUT"     -- как аргумент этой опции называется в документации
  <> help "Input file" ) --

-- Можно не использовать флаг i, а просто написать входную строку (1+2 в stack run exe -- 1+2)
strInput :: Parser Input
strInput = StrInput <$> strArgument (metavar "STRING" <> help "String to be parsed")

-- Парсер аргумента, специфицирующий, куда писать результат работы программы
outputParser :: Parser Output
outputParser = fileOutput <|> defaultOutput

-- Флаг -o/--output позволяет задать строку -- имя выходного файла
fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
  (  short 'o'
  <> long "output"
  <> metavar "OUTPUT"
  <> help (printf "Output file. If not specified, output is INPUT.out. If INPUT is not specified, output is %s" defaultOutFile)
  )

-- Если флаг -o не использован, выбираем дефолтное имя выходного файла
defaultOutput :: Parser Output
defaultOutput = pure DefaultOutput

-- Флаг, специфицирующий, надо ли печатать результат работы приложения в консоль
dumpToStdoutParser :: Parser Bool
dumpToStdoutParser = switch
  (  short 'd'
  <> help "Render input and output into stdout"
  )

-- Вспомогательная функция, подготавливающая входную строку -- из файла или непосредственно аргумента командной строки
getInput :: Input -> IO String
getInput (FileInput path) = readFile path
getInput (StrInput str) = return str

-- Вспомогательная функция, подготавливающая имя файла для результата
getOutput :: Output -> Input -> IO FilePath
getOutput (FileOutput path) _ = return path -- если путь указан, берем его
getOutput _ (FileInput path) = return $ path <.> "out" -- иначе, если вход был из файла, добавляем к его имени out
getOutput _ _ = return defaultOutFile -- иначе берем дефолтное имя файла (bad style, не делайте так)

-- Очень плохо, не надо так
defaultOutFile :: String
defaultOutFile = "str.out"

-- Функция, запускающая правильный парсер и записывающая результат работы в выходной файл
runEcho :: String -> FilePath -> IO ()
runEcho s path =
  writeFile path (printf "%s" s)

-- Функция, которая выводит результат работы программы в консоль
dumpIntoStdout :: Bool -> String -> FilePath -> IO ()
dumpIntoStdout False _ _ = return ()
dumpIntoStdout True i o = do
  out <- readFile o
  putStrLn $ printf "===================================\nInput:\n\n%s\n-----------------------------------\nOutput:\n\n%s\n===================================\n" i out
