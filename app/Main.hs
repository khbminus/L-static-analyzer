module Main (main) where

import Statement(Statement(Write, Skip, Read), Expression(Const, VariableName))
import Execute (run)
import Context (emptyContext)

main :: IO ()
main = do
    let writeConst = Write (Const 1)
    let writeVar = Write (VariableName "var")
    let skip = Skip
    let readVar = Read "var"

    run emptyContext [readVar, writeVar]
    run emptyContext [readVar]
    run emptyContext [writeVar]
    run emptyContext [writeConst]
    run emptyContext [skip]

