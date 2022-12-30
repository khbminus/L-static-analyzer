module Grammar where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Void
import Statement (Expression (..), Function (..), Operations (..), Statement (..), reservedKeywords)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Error (ParsecError)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

number :: Parser Int
number = lexeme L.decimal <?> "number"

constValue :: Parser Expression
constValue = Const <$> lexeme L.decimal <?> "const value"

name :: Parser String
name = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar <?> "Variable"
    check x -- TODO: check for function names are required due to `f argument1 argument2 1` issue
      | x `elem` reservedKeywords = fail $ "keyword " ++ show x ++ " cannot be an identifier"
      | otherwise = return x

varName :: Parser Expression
varName = VariableName <$> name

funCall :: Parser Expression
funCall = do
  FunctionCall <$> (lexeme name <?> "Function name") <*> (lexeme . parens) (arguments <?> "arguments")
  where
    arguments :: Parser [Expression]
    arguments = expression `sepBy` lexeme (symbol ",")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyParens :: Parser a -> Parser a
curlyParens = between (symbol "{") (symbol "}")

expressionTerm :: Parser Expression
expressionTerm =
  choice
    [ parens expression,
      try funCall,
      varName,
      constValue
    ]

expressionOperationsTable :: [[Operator Parser Expression]]
expressionOperationsTable =
  [ [ binary "*" $ Application Multiplication,
      binary "/" $ Application Division,
      binary "%" $ Application Modulo
    ],
    [ binary "+" $ Application Addition,
      binary "-" $ Application Subtraction
    ],
    [ binary "==" $ Application Equals,
      binary "!=" $ Application NotEquals,
      binary "<"  $ Application Less,
      binary "<=" $ Application LessOrEquals,
      binary ">=" $ Application GreaterOrEquals,
      binary ">"  $ Application Greater
    ],
    [ binary "&&" $ Application LazyAnd
    ],
    [ binary "||" $ Application LazyOr
    ]
  ]
  where
    binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
    binary name f = InfixL (f <$ symbol name)

expression :: Parser Expression
expression = makeExprParser expressionTerm expressionOperationsTable

singleton :: a -> [a]
singleton x = [x]

letVariable :: Parser [Statement]
letVariable = singleton <$> (Let <$> (lexeme name <?> "Variable name") <*> (symbol ":=" *> expression) <?> "Variable let")

write :: Parser [Statement]
write = singleton . Write <$> (symbol "write" *> expression) <?> "write statement"

readVariable :: Parser [Statement]
readVariable = singleton . Read <$> (symbol "read" *> name <?> "Read statement")

while :: Parser [Statement]
while =
  singleton
    <$> ( While
            <$> (between (symbol "while") (symbol "do") expression <?> "While condition")
            <*> (curlyParens statement <?> "While statement")
        )

ifThenElse :: Parser [Statement]
ifThenElse =
  singleton
    <$> ( If
            <$> (symbol "if" *> expression <?> "If condition")
            <*> (symbol "then" *> curlyParens statement <?> "True statement")
            <*> (symbol "else" *> curlyParens statement <?> "False Statement")
        )

funCallStatement :: Parser [Statement]
funCallStatement =
  singleton
    <$> ( FunctionCallStatement
            <$> (lexeme name <?> "Function name")
            <*> (lexeme . parens) (arguments <?> "arguments")
        )
  where
    arguments :: Parser [Expression]
    arguments = expression `sepBy` lexeme (symbol ",")

functionDeclaration :: Parser [Statement]
functionDeclaration =
  buildDeclaration
    <$> (symbol "def" *> name)
    <*> parens (name `sepBy` symbol ",")
    <*> curlyParens statement
    <*> optional (symbol "return" *> expression)
  where
    buildDeclaration a b c d = [FunctionDeclaration a (Function b c d)]

skip :: Parser [Statement]
skip = [Skip] <$ symbol "skip"

split :: Parser [Statement]
split = concat <$> (statement `sepBy1` symbol ";")

statement :: Parser [Statement]
statement = concat <$> (terms `sepBy1` symbol ";")
  where
    terms =
      choice
        [ ifThenElse,
          while,
          write,
          readVariable,
          skip,
          try funCallStatement,
          functionDeclaration,
          letVariable
        ]

parseStatement :: String -> Either (ParseErrorBundle String Void) [Statement]
parseStatement = parse (statement <* eof) ""

data REPLInput = ConsoleStatement [Statement]
               | ConsoleExpression Expression
               deriving (Eq, Show)

statementOrExpression :: Parser REPLInput
statementOrExpression = fmap ConsoleStatement (try statement) <|> fmap ConsoleExpression expression

parseStatementOrExpression :: String -> Either ParsecError REPLInput
parseStatementOrExpression = parse (statementOrExpression <* eof) ""
