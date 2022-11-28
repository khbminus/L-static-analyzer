module Grammar where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Void
import Statement (Expression (..), Operations (..), Statement (..), reservedKeywords)
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

number :: Parser Int
number = lexeme L.decimal <?> "number"

constValue :: Parser Expression
constValue = Const <$> lexeme L.decimal <?> "const value"

name :: Parser String
name = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar <?> "Variable"
    check x
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
  [ [ binary "*" $ compose Multiplication,
      binary "/" $ compose Division,
      binary "%" $ compose Modulo
    ],
    [ binary "+" $ compose Addition,
      binary "-" $ compose Subtraction
    ],
    [ binary "==" $ compose Equals,
      binary "!=" $ compose NotEquals,
      binary "<" $ compose Less,
      binary "<=" $ compose LessOrEquals,
      binary ">=" $ compose GreaterOrEquals,
      binary ">" $ compose Greater
    ],
    [ binary "&&" $ compose LazyAnd
    ],
    [ binary "||" $ compose LazyOr
    ]
  ]
  where
    binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
    binary name f = InfixL (f <$ symbol name)

    compose :: (Expression -> Expression -> Operations) -> Expression -> Expression -> Expression
    compose f a b = Application $ f a b

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
            <*> (statement <?> "While statement")
        )

ifThenElse :: Parser [Statement]
ifThenElse =
  singleton
    <$> ( If
            <$> (symbol "if" *> expression <?> "If condition")
            <*> (symbol "then" *> statement <?> "True statement")
            <*> (symbol "else" *> statement <?> "False Statement")
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

skip :: Parser [Statement]
skip = [Skip] <$ symbol "skip"

split :: Parser [Statement]
split = concat <$> (statement `sepBy1` symbol ";")

statement :: Parser [Statement]
statement =
  try while <|> try ifThenElse
    <|> (concat <$> (terms `sepBy1` symbol ";"))
  where
    terms =
      choice
        [ write,
          readVariable,
          skip,
          try funCallStatement,
          letVariable
        ]
