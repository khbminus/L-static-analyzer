module Grammar where

import Control.Monad
import Data.Void
import Statement (Expression (..), Statement (..), reservedKeywords)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pConst :: Parser Expression
pConst = Const <$> lexeme L.decimal <?> "const value"

pName :: Parser String
pName = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar <?> "Variable"
    check x
      | x `elem` reservedKeywords = fail $ "keyword " ++ show x ++ " cannot be an identifier"
      | otherwise = return x

pVarName :: Parser Expression
pVarName = VariableName <$> pName

pFunctionCall :: Parser Expression
pFunctionCall = do
  FunctionCall <$> (lexeme pName <?> "Function name")  <*> (arguments <?> "arguments")
  where
    arguments :: Parser [Expression]
    arguments = (:) <$> pExpression <*> many pExpression

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pExpression :: Parser Expression
pExpression =
  choice
    [ parens pExpression,
      try pFunctionCall,
      pVarName, -- TODO: "a " should raise function without arguments error, not var parsing error
      pConst
    ]

letVariable :: Parser Statement
letVariable =
  Let <$> (lexeme pName <?> "Variable name") <*> (symbol ":=" *> pExpression) <?> "Variable let"

write :: Parser Statement
write = do
  Write <$> (symbol "write" *> pExpression) <?> "while statement"

readVariable :: Parser Statement
readVariable = do
  Read <$> (symbol "read" *> pName <?> "Read statement")

while :: Parser Statement
while =
  While
    <$> (between (symbol "while") (symbol "do") pExpression <?> "While condition")
    <*> (statement <?> "While statement")

ifThenElse :: Parser Statement
ifThenElse =
  If
    <$> (symbol "if" *> pExpression <?> "If condition")
    <*> (symbol "then" *> statement <?> "True statement")
    <*> (symbol "else" *> statement <?> "False Statement")

functionCallStatement :: Parser Statement
functionCallStatement =
  FunctionCallStatement
    <$> (pName <?> "function name")
    <*> (arguments <?> "arguments")
  where
    arguments :: Parser [Expression]
    arguments = (:) <$> pExpression <*> many pExpression

statement :: Parser Statement
statement =
  choice
    [ write,
      readVariable,
      while,
      ifThenElse,
      try functionCallStatement,
      letVariable
    ]
