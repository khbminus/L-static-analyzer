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

joinToString :: Show a => String -> [a] -> String
joinToString _ [x]        = show x
joinToString delim (x:xs) = show x ++ delim ++ joinToString delim xs
joinToString _ []         = ""

bodyToString :: [Statement] -> String
bodyToString body = "{ " ++ case body of
  [] -> "skip"
  b  -> joinToString "; " b
  ++ " }"

instance Show Statement where
  show (Let x expr) = x ++ " := " ++ show expr
  show (FunctionCallStatement name args) = name ++ "(" ++ show args ++ ")"
  show (FunctionDeclaration name f) = "def " ++ name ++ show f
  show (Write x) = "write " ++ show x
  show (Read x) = "read " ++ x
  show (While e s) = "while (" ++ show e ++ ") " ++ bodyToString s
  show (If e t f) = "If " ++ show e ++ " then " ++ bodyToString t ++ " else " ++ bodyToString f
  show Skip = "skip"

data Function = Function [String] [Statement] (Maybe Expression) deriving (Eq)

instance Show Function where
  show (Function args body Nothing)    = "(" ++ joinToString ", " args ++ ") " ++ bodyToString body
  show (Function args body (Just ret)) = "(" ++ joinToString ", " args ++ ") " ++ bodyToString body ++ " return " ++ show ret

reservedKeywords :: [String]
reservedKeywords = ["if", "then", "else", "while", "do", "read", "write"]