module Statement where

data Operations
  = Addition
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
  | LazyOr
  deriving (Eq, Show)

data Expression
  = FunctionCall String [Expression]
  | VariableName String
  | Const Int
  | Application ApplicationT
  deriving (Show, Eq)

data ApplicationT = ApplicationT {leftOperand :: Expression, op :: Operations, rightOperand :: Expression} deriving (Show, Eq)

data Statement
  = Let String Expression
  | FunctionCallStatement String [Expression]
  | Write Expression
  | Read String
  | While Expression Statement
  | If Expression Statement Statement
  deriving (Show, Eq)

reservedKeywords :: [String]
reservedKeywords = ["if", "then", "else", "while", "do", "read", "write"]