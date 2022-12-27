{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Analysis.IR where
import Compiler.Hoopl
import Statement (Expression(..))

type M = CheckingFuelMonad SimpleUniqueMonad -- Magic

data Proc = Proc { name :: String, args :: [String], entry :: Label, body :: Graph Instruction C C }

data Instruction e x where
  Label :: Label ->                     Instruction C O
  Let :: String -> Expression ->        Instruction O O
  If :: Expression -> Label -> Label -> Instruction O C -- if (!expr) goto
  Return :: [Expression] ->             Instruction O C
  Goto :: Label ->                      Instruction O C
  Write :: Expression ->                Instruction O O
  Read :: String ->                     Instruction O O
  Skip ::                               Instruction O O
  -- Call :: FIXME

instance NonLocal Instruction where
  entryLabel :: Instruction C x -> Label
  entryLabel (Label l) = l
  entryLabel _ = error "Entry label for not label" -- make GHC happy

  successors (Goto l) = [l]
  successors (If _ t f) = [t, f]
  successors _ = error "Successor of not sucessorable thing" -- make GHC happy

instance Show (Instruction e x) where
  show (Label l) = show l ++ ": "
  show (Let x expr) = indent $ show x ++ " := " ++ show expr
  show (If e t f) = indent $ "if " ++ show e ++ "then goto " ++ show t ++ "else goto " ++ show f
  show (Goto l) = indent $ "goto" ++ show l
  show (Write expr) = indent $ "write " ++ show expr
  show (Read var) = indent $ "read " ++ var
  show Skip = indent  "Skip"
  show (Return expr) = indent $ "return" ++ show expr


indent :: String -> String
indent x = "  " ++ x