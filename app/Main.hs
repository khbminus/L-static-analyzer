module Main (main) where

import Statement(Statement(Write, Skip, Read), Expression(Const, VariableName))
--import Execute (run)
import Context (empty)

main :: IO ()
main = do
    let writeConst = Write (Const 1)
    let writeVar = Write (VariableName "var")
    let skip = Skip
    let readVar = Read "var"

    print readVar
--    run singleton [readVar, writeVar]
--    run singleton [readVar]
--    run singleton [writeVar]
--    run singleton [writeConst]
--    run singleton [skip]

