module Execute (run, execute) where

import Context (Context (..))
import Control.Monad.State
import Error (RuntimeError (..))
import Evaluate (evaluateStatements)
import Grammar (statement)
import Text.Megaparsec (eof, parse)
import Data.Maybe (isNothing)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void
import Statement (Statement)
import ConsoleParser (REPLInput, statementOrExpression)
import Context (setErrorT)

parseStatement :: String -> Either (ParseErrorBundle String Void) [Statement]
parseStatement = parse (statement <* eof) ""

run :: [String] -> StateT Context IO ()
run = foldr ((>>) . execute) (return ())

execute :: String -> StateT Context IO ()
execute str = do
  context <- get
  guard ( isNothing (Context.error context) )
  case parseStatement str of
    Left err -> setErrorT $ ParserError err
    Right statements -> evaluateStatements statements
