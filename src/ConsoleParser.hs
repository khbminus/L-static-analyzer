module ConsoleParser where

import qualified Options.Applicative as Optparse
import qualified Text.Megaparsec as Megaparsec
import qualified Grammar as LParser
import Text.Megaparsec ( (<?>), (<|>) )
import Context (VarContext, setVarContext, emptyVarContext)

-- Тип данных, агрегирующий все аргументы командной строки, возвращается actionParser-ом
data Action = Action
            { input :: Input
            , liveness :: Bool 
            , vars :: [String]
            , extend :: Bool
            , ir :: Bool
            }
            deriving (Show)

-- Парсер аргументов командной строки
actionParser :: Optparse.Parser Action
actionParser = Action <$> (inputParser <|> pure Interactive) <*> liveOptParser <*> varsParser <*> extendParser <*> irParser

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

liveOptParser :: Optparse.Parser Bool
liveOptParser = Optparse.switch 
  (  Optparse.long "liveness-optimization"
  <> Optparse.help "Whether to enable liveness optimization" )

extendParser :: Optparse.Parser Bool
extendParser = Optparse.switch
  (  Optparse.long "extend"
  <> Optparse.short 'E'
  <> Optparse.help "Print instructions to execute, but do not execute code" )

irParser :: Optparse.Parser Bool
irParser = Optparse.switch
  (  Optparse.long "ir"
  <> Optparse.help "Print IR of code" )

varsParser :: Optparse.Parser [String]
varsParser = Optparse.many $ Optparse.argument Optparse.str $ Optparse.metavar "VARS..."

varArgParser :: LParser.Parser (String, Int)
varArgParser = (,) 
    <$> (LParser.lexeme LParser.name <?> "Variable name") 
    <*> (LParser.symbol "=" *> LParser.lexeme LParser.number <?> "const value" ) <?> "Variable argument"

getVarContext :: [String] -> VarContext
getVarContext (x:xs) = 
  let res = Megaparsec.parse varArgParser "" x in
  case res of
    Left err -> Prelude.error $ show err 
    Right (var, val) -> setVarContext var val (getVarContext xs)
getVarContext [] = emptyVarContext

