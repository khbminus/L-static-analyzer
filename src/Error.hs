module Error where
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)

type ParsecError = ParseErrorBundle String Void

data RuntimeError = ParserError ParsecError
                  | VarNotFound String
                  | FunctionNotFound String
                  | UnexpectedEOF
                  | CallOfVoidFunctionInExpression String
                  | InvalidNumberOfArguments String Int Int
                  | InvalidInput String
                  deriving (Show, Eq)