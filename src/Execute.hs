module Execute(run) where

import Context (Context (..))
import Control.Monad.State.Lazy
import Evaluate (evaluateStatements)
import Grammar (statement)
import Text.Megaparsec (runParser, eof)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)

run :: String -> [String] -> State Context (Maybe (ParseErrorBundle String Void))
run _ [] = return Nothing
run fileName (x : xs) =
      case runParser (statement <* eof) fileName x of
        Left err -> return $ Just err
        Right statements ->
          do
            evaluateStatements statements
            run fileName xs
