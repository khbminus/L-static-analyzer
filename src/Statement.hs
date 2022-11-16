module Statement where

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
                  | Application ApplicationT deriving (Show, Eq)

  data FunctionCallT = FunctionCallT { functionName :: String, arguments :: [Expression] } deriving (Show, Eq)
  data ApplicationT = ApplicationT { leftOperand :: Expression, op :: Operations, rightOperand :: Expression } deriving (Show, Eq)
  data IfT = IfT { condition :: Expression, statementTrue :: Statement, statementFalse :: Statement} deriving (Show, Eq)

  data Statement = Let String Expression
                 | FunctionCallStatement FunctionCallT
                 | Write Expression
                 | Read Expression
                 | While Expression Statement
                 | If IfT deriving (Show, Eq)
