module Execute (run) where

import Context (Context (..))
import Control.Monad.State
import Error (RuntimeError (..))
import Evaluate (evaluateStatements)
import Grammar (statement)
import Text.Megaparsec (eof, runParser)

run :: [String] -> StateT Context IO ()
run [] = return ()
run (x : xs) = do
  cxt <- get
  guard ( Context.error cxt == Nothing )
  case runParser (statement <* eof) "" x of
    Left err -> put $ cxt { Context.error = Just $ ParserError err }
    Right statements -> do
      evaluateStatements statements
      run xs
