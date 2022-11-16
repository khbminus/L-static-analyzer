module Statement(Operations, Expression, FunctionCallT, ApplicationT, IfT, Statement) where

  data Operations = Addition
                  | Subtraction
                  | Multiplication
                  | Division
                  | Modulo
                  | Equals
                  | NotEquals
                  | Greater
                  | GreaterOrEquals
                  | Less
                  | LessOrEquals
                  | LazyAnd
                  | LazyOr deriving (Eq, Show)


  data Expression = FunctionCall FunctionCallT
                  | VariableName String
                  | Const Int
                  | Application ApplicationT

  data FunctionCallT = FunctionCallT { functionName :: String, arguments :: [Expression] }
  data ApplicationT = ApplicationT { leftOperand :: Expression, op :: Operations, rightOperand :: Expression }
  data IfT = IfT { condition :: Expression, statementTrue :: Statement, statementFalse :: Statement}

  data Statement = Let String Expression
                 | FunctionCallStatement FunctionCallT
                 | Write Expression
                 | Read Expression
                 | While Expression Statement
                 | If IfT
