module Execute(run) where

import Context (Context (..), InputSource (..))
import Control.Monad.State.Lazy
import Evaluate (evaluateStatements)
import Grammar (statement)
import Text.Megaparsec (runParser, eof)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)

run :: State Context (Maybe (ParseErrorBundle String Void))
run = do
  ctx <- get
  case (inputLines . input) ctx of
    [] -> return Nothing
    (x : xs) -> do
      case runParser (statement <* eof) (fileName $ input ctx) x of
        Left err -> return $ Just err
        Right statements ->
          do
            put ctx {input = (input ctx) {inputLines = xs}}
            evaluateStatements statements
            run
