module Main (main) where

import Statement(Statement(Write, Skip, Read), Expression(Const, VariableName))
import Execute (run)
import Context (newContext)
import Control.Monad.State

main :: IO ()
main = do
  let writeConst = Write (Const 1)
  let writeVar = Write (VariableName "var")
  let skip = Skip
  let readVar = Read "var"

  evalStateT (run ["read var", "write var"]) newContext
