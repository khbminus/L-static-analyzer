module ConsoleParser where

import qualified Options.Applicative as Optparse
import qualified Text.Megaparsec as Megaparsec
import qualified Grammar as LParser
import Text.Megaparsec ( (<?>), (<|>) )
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
actionParser = Action <$> (inputParser <|> pure Interactive) <*> varsParser

-- Тип входных данных
data Input = FileInput FilePath -- Имя входного файла
           | Interactive
           deriving (Show)

-- Парсер аргумента, специфицирующий, откуда брать входные данные
-- Флаг -i/--input позволяет задать строку -- имя входного файла
inputParser :: Optparse.Parser Input
inputParser = FileInput <$> Optparse.strOption
  (  Optparse.short 'i'           -- короткое имя флага (-i)
  <> Optparse.long "input"        -- длинное имя флага (--input)
  <> Optparse.metavar "INPUT"     -- как аргумент этой опции называется в документации
  <> Optparse.help "Input file" )

varsParser :: Optparse.Parser [String]
varsParser = Optparse.many $ Optparse.argument Optparse.str $ Optparse.metavar "VARS..."

varArgParser :: LParser.Parser (String, Int)
varArgParser = (,) 
    <$> (LParser.lexeme LParser.name <?> "Variable name") 
    <*> (LParser.symbol "=" *> LParser.lexeme LParser.decimal <?> "const value" ) <?> "Variable argument"

getVarContext :: [String] -> VarContext
getVarContext (x:xs) = 
  let res = Megaparsec.parse varArgParser "" x in
  case res of
    Left err -> Prelude.error $ show err 
    Right (var, val) -> setVarContext (getVarContext xs) var val
getVarContext [] = emptyVarContext

getInput :: Input -> IO String
getInput (FileInput path) = readFile path
getInput Interactive = getLine

runInterpreter :: VarContext -> [Statement] -> IO ()
runInterpreter varcxt = run $ emptyContext { Context.vars = varcxt }
