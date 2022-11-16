module Grammar where
import Data.Void
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Statement (Expression(..))

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


pInteger :: Parser Expression
pInteger = Const <$> lexeme L.decimal

pVarName :: Parser Expression
pVarName = VariableName <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "Variable")