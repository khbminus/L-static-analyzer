module Statement where

data Operations
  = Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | Modulo Expression Expression
  | Equals Expression Expression
  | NotEquals Expression Expression
  | Greater Expression Expression
  | GreaterOrEquals Expression Expression
  | Less Expression Expression
  | LessOrEquals Expression Expression
  | LazyAnd Expression Expression
  | LazyOr Expression Expression
  deriving (Eq, Show)

data Expression
  = FunctionCall String [Expression]
  | VariableName String
  | Const Int
  | Application Operations
  deriving (Show, Eq)

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