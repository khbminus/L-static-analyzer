module Error where
import Statement(Expression)

data RuntimeError = EvalError Expression
                  | VarNameError String
                  | UnsupportedError
                  | InvalidInputError String
                  deriving (Show, Eq)
