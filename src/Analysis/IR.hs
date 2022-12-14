{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Analysis.IR(Instruction(..), Proc(..), M) where
import Compiler.Hoopl
import Statement (Expression(..))

type M = CheckingFuelMonad SimpleUniqueMonad -- Magic

data Proc = Proc { name :: String, args :: [String], entry :: Label, body :: Graph Instruction C C }

data Instruction e x where
  Label  :: Label ->                           Instruction C O
  Let    :: String -> Expression ->            Instruction O O
  Write  :: Expression ->                      Instruction O O
  Read   :: String ->                          Instruction O O
  Skip   ::                                    Instruction O O
  If     :: Expression -> Label -> Label ->    Instruction O C
  Return :: Maybe Expression ->                Instruction O C
  Goto   :: Label ->                           Instruction O C
  While  :: Expression -> Label -> Label ->    Instruction O C
  Call   :: String -> [Expression] -> Label -> Instruction O C -- accidentally should be OC 

instance NonLocal Instruction where
  entryLabel :: Instruction C x -> Label
  entryLabel (Label l) = l

  successors :: Instruction e C -> [Label]
  successors (Goto l) = [l]
  successors (If _ t f) = [t, f]
  successors (Call _ _ l) = [l]
  successors (Return _) = []
  successors (While _ start next) = [start, next]

instance Show (Instruction e x) where
  show :: Instruction e x -> String
  show (Label l) = show l ++ ": "
  show (Let x expr) = indent $ show x ++ " := " ++ show expr
  show (If e t f) = indent $ "if " ++ show e ++ " then goto " ++ show t ++ " else goto " ++ show f
  show (Goto l) = indent $ "goto " ++ show l
  show (Write expr) = indent $ "write " ++ show expr
  show (Read var) = indent $ "read " ++ var
  show Skip = indent  "Skip"
  show (Return Nothing) = indent $ "return"
  show (Return (Just x)) = indent $ "return " ++ show x
  show (Call name args toLabel) = indent $ "call " ++ name ++ "(" ++ show args ++ " -> " ++ show toLabel
  show (While expr start next) = indent $ "while (" ++ show expr ++ ") goto " ++ show start ++ " after goto " ++ show next

indent :: String -> String
indent x = "  " ++ x