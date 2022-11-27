module Execute (run) where

import Context (Context (..))
import Control.Monad.State
import Error (RuntimeError (..))
import Evaluate (evaluateStatements)
import Grammar (statement)
import Text.Megaparsec (eof, runParser)

run :: String -> [String] -> StateT Context (Either RuntimeError) ()
run _ [] = return ()
run fileName (x : xs) = do
  case runParser (statement <* eof) fileName x of
    Left err -> lift $ Left $ ParserError err
    Right statements -> do
      evaluateStatements statements
      run fileName xs
