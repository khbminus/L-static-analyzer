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

parseInput :: String -> Either (ParseErrorBundle String Void) [Statement]
parseInput = parse (statement <* eof) ""

run :: [String] -> StateT Context IO ()
run = foldr ((>>) . execute) (return ())

execute :: String -> StateT Context IO ()
execute str = do
  context <- get
  guard ( isNothing (Context.error context) )
  case parseInput str of
    Left err -> put $ context { Context.error = Just $ ParserError err }
    Right statements -> evaluateStatements statements
