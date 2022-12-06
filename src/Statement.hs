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
  | FunctionDeclaration String Function
  | Write Expression
  | Read String
  | While Expression [Statement]
  | If Expression [Statement] [Statement]
  | Skip
  deriving (Show, Eq)

data Function = Function [String] [Statement] (Maybe Expression) deriving (Show, Eq)

reservedKeywords :: [String]
reservedKeywords = ["if", "then", "else", "while", "do", "read", "write"]