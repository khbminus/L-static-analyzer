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
  deriving (Eq)

instance Show Operations where
  show Addition = "+"
  show Subtraction = "-"
  show Multiplication = "*"
  show Division = "/"
  show Modulo = "%"
  show Equals = "=="
  show NotEquals = "!="
  show Greater = ">"
  show Less = "<"
  show GreaterOrEquals = ">="
  show LessOrEquals = "<="
  show LazyAnd = "&&"
  show LazyOr = "||"

data Expression
  = FunctionCall String [Expression]
  | VariableName String
  | Const Int
  | Application Operations Expression Expression
  deriving (Eq)

instance Show Expression where
  show (FunctionCall name args) = name ++ "(" ++ show args ++ ")"
  show (VariableName x) = x
  show (Const x) = show x
  show (Application op l r) = parens l ++ " " ++ show op ++ " " ++ parens r
    where
      parens :: Expression -> String
      parens (Application op l r) = "(" ++ show (Application op l r) ++ ")"
      parens x = show x

data Statement
  = Let String Expression
  | FunctionCallStatement String [Expression]
  | FunctionDeclaration String Function
  | Write Expression
  | Read String
  | While Expression [Statement]
  | If Expression [Statement] [Statement]
  | Skip
  deriving (Eq)

instance Show Statement where
  show (Let x expr) = x ++ " := " ++ show expr
  show (FunctionCallStatement name args) = name ++ "(" ++ show args ++ ")"
  show (FunctionDeclaration name f) = "def " ++ name ++ show f
  show (Write x) = "write " ++ show x
  show (Read x) = "read " ++ x
  show (While e s) = "while (" ++ show e ++ ") {" ++ show s ++ "}"
  show (If e t f) = "If " ++ show e ++ " then " ++ show t ++ " else " ++ show f
  show Skip = "skip"

data Function = Function [String] [Statement] (Maybe Expression) deriving (Eq)

instance Show Function where
  show (Function args body Nothing) = "(" ++ show args ++ ") {" ++ show body ++ "}"
  show (Function args body (Just ret)) = "(" ++ show args ++ ") {" ++ show body ++ "} return " ++ show ret

reservedKeywords :: [String]
reservedKeywords = ["if", "then", "else", "while", "do", "read", "write"]