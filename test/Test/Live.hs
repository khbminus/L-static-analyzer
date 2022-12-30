{-# LANGUAGE NamedFieldPuns #-}
module Test.Live where

import Statement (Statement (..), Function (Function), Expression (..), Operations (Equals, Addition))
import Grammar (parseStatement)
import Data.Either (isLeft, fromRight)
import Data.Maybe (fromJust)
import Analysis.Live (optimizeLive)

import Test.Tasty.HUnit (assertEqual)


parse :: String -> Maybe [Statement]
parse str = if any isLeft parsed
    then Nothing
    else Just $ foldl f [] parsed
    where
        parse' ls = map parseStatement ls
        parsed = parse' $ lines str
        f b a = b ++ fromRight [] a

optimizeFromString :: String -> [Statement]
optimizeFromString text = case parse text of
    Nothing -> error "Parsing error"
    Just ir -> optimizeLive ir

increment :: String -> Expression -> Statement
increment var expr = Let var (Application Addition (VariableName var) expr)

unit_Liveness :: IO ()
unit_Liveness = do
    let testCode1 = "def f() { x := 5; y := x } return 1"
    let expected1 = [FunctionDeclaration "f" (Function [] [] (Just $ Const 1))]

    let testCode2 = "def f() { x := 5; y := 1 } return x"
    let expected2 = [FunctionDeclaration "f" (Function [] [Let "x" (Const 5)] (Just $ VariableName "x"))]

    let testCode3 = "def f() { x := 5; x := 1 } return x"
    let expected3 = [FunctionDeclaration "f" (Function [] [Let "x" (Const 1)] (Just $ VariableName "x"))]


    assertEqual "Liveness 1" (optimizeFromString testCode1) expected1
    assertEqual "Liveness 2" (optimizeFromString testCode2) expected2
    assertEqual "Liveness 3" (optimizeFromString testCode3) expected3

unit_ReadWrite :: IO ()
unit_ReadWrite = do
    let testCode1 = "def f() { read x }"
    let expected1 = [FunctionDeclaration "f" (Function [] [Read "x"] Nothing)]

    let testCode2 = "def f() { x := 0; write x }"
    let expected2 = [FunctionDeclaration "f" (Function [] [Let "x" $ Const 0, Write $ VariableName "x"] Nothing)]

    assertEqual "Liveness 1" (optimizeFromString testCode1) expected1
    assertEqual "Liveness 2" (optimizeFromString testCode2) expected2

unit_If :: IO ()
unit_If = do
    let testCode1 = "def f() { y := 0; z := 0; if z == 0 then { y := y + 3 } else { z := z + 1 } } return y"
    let expected1 = [FunctionDeclaration "f" (Function [] [Let "y" (Const 0), Let "z" (Const 0), If (Application Equals (VariableName "z") (Const 0)) [increment "y" (Const 3)] []] (Just $ VariableName "y"))]
    
    let testCode2 = "def f() { y := 0; z := 0; if z == 0 then { y := y + 3 } else { z := z + 1 } } return y + z"
    let expected2 = fromJust $ parse testCode2

    assertEqual "remove branch" (optimizeFromString testCode1) expected1
    assertEqual "unchanged"     (optimizeFromString testCode2) expected2

unit_While :: IO ()
unit_While = do
    let testCode1 = "def f() { x := 0; while 0 == 0 do { x := x + 3 } } return x"
    let expected1 = fromJust $ parse testCode1

    let testCode2 = "def f() { x := 0; while 0 == 0 do { x := x + 3 } }"
    let expected2 = [FunctionDeclaration "f" $ Function [] [While (Application Equals (Const 0) (Const 0)) []] Nothing]

    assertEqual "unchanged" (optimizeFromString testCode1) expected1
    assertEqual "remove x"  (optimizeFromString testCode2) expected2
