module CommandLineParser where

import qualified Options.Applicative as Optparse
import qualified Text.Megaparsec as Megaparsec
import qualified Grammar as LParser
import Text.Megaparsec ( (<?>) )
import Context (VarContext, Context(..), setVarContext, emptyContext, emptyVarContext)
import Statement
import Execute (run)

-- Тип данных, агрегирующий все аргументы командной строки, возвращается actionParser-ом
data Action = Action
            { input :: Input
            , vars :: [String] }
            deriving (Show)


-- Парсер аргументов командной строки
actionParser :: Optparse.Parser Action
actionParser = Action <$> inputParser <*> varsParser

-- Тип входных данных
data Input = FileInput FilePath -- Имя входного файла
           deriving (Show)

-- Парсер аргумента, специфицирующий, откуда брать входные данные
inputParser :: Optparse.Parser Input
inputParser = fileInput

varsParser :: Optparse.Parser [String]
varsParser = Optparse.many $ Optparse.argument Optparse.str $ Optparse.metavar "VARS..."

varArg :: LParser.Parser (String, Int)
varArg = (,) 
    <$> (LParser.lexeme LParser.name <?> "Variable name") <*> (LParser.symbol "=" *> LParser.lexeme LParser.decimal <?> "const value" ) <?> "Variable argument"


getVarContext :: [String] -> VarContext
getVarContext (x:xs) = 
  let res = Megaparsec.parse varArg "" x in
  case res of
    Left err -> getVarContext xs
    Right (var, val) -> setVarContext (getVarContext xs) var val
getVarContext [] = emptyVarContext

-- Флаг -i/--input позволяет задать строку -- имя входного файла
fileInput :: Optparse.Parser Input
fileInput = FileInput <$> Optparse.strOption --
  (  Optparse.short 'i'           -- короткое имя флага (-i)
  <> Optparse.long "input"        -- длинное имя флага (--input)
  <> Optparse.metavar "INPUT"     -- как аргумент этой опции называется в документации
  <> Optparse.help "Input file" ) --

-- Вспомогательная функция, подготавливающая входную строку -- из файла или непосредственно аргумента командной строки
getInput :: Input -> IO String
getInput (FileInput path) = readFile path

-- Функция, запускающая парсер и записывающая результат работы в стандартный вывод
runInterpreter :: VarContext -> [Statement] -> IO ()
runInterpreter varcxt = run $ emptyContext { Context.vars = varcxt }

