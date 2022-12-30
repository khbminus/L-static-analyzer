{-# LANGUAGE NamedFieldPuns #-}
module Test.Live where

import Compiler.Hoopl
import Analysis.IR (Proc (..), M, Instruction (Return))
import Statement (Statement (..), Function (Function), Expression (..), Operations (Equals, Addition))
import Grammar (parseStatement, ifThenElse)
import Data.Either (isLeft, fromRight)
import Data.Maybe (fromJust)
import Analysis.AstToIr (astToIR)
import Analysis.IrToAst (irToAst)
import Analysis.Live (liveLattice, liveness, deadAsstElim)

import Test.Tasty.HUnit (assertBool, assertEqual)

type ErrorM = Either String

liveOpt :: M [Proc] -> ErrorM (M [Proc])
liveOpt procs =
    return $ procs >>= mapM optProc
  where
    optProc proc@Proc {entry, body, args} = do
        (body', _, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) body mapEmpty
        return $ proc { body = body' }
    bwd = BwdPass { bp_lattice  = liveLattice
                  , bp_transfer = liveness
                  , bp_rewrite  = deadAsstElim
                  }

parse :: String -> Maybe [Statement]
parse str = if any isLeft parsed
    then Nothing
    else Just $ foldl f [] parsed
    where
        parse' ls = map parseStatement ls
        parsed = parse' $ lines str
        f b a = b ++ fromRight [] a

optimize :: String -> [Statement]
optimize text = do
    case fmap astToIR (parse text) of
        Nothing -> error "Parsing error"
        Just ir -> case liveOpt (fmap snd ir) of
            Left err -> error err
            Right p  -> do
                let opted = runSimpleUniqueMonad $ runWithFuel fuel p
                    -- lbmaps = runSimpleUniqueMonad $ runWithFuel fuel (liftM (fst . unzip) p)
                    -- expected = runSimpleUniqueMonad $ runWithFuel fuel exps
                -- TODO: get Instructions from [Proc]
                irToAst opted
  where
    fuel = 9999

unit_Liveness :: IO ()
unit_Liveness = do
    let testCode1 = "def f() { x := 5; y := x } return 1"
    let expected1 = [FunctionDeclaration "f" (Function [] [] (Just $ Const 1))]

    let testCode2 = "def f() { x := 5; y := 1 } return x"
    let expected2 = [FunctionDeclaration "f" (Function [] [Let "x" (Const 5)] (Just $ VariableName "x"))]

    let testCode3 = "def f() { x := 5; x := 1 } return x"
    let expected3 = [FunctionDeclaration "f" (Function [] [Let "x" (Const 1)] (Just $ VariableName "x"))]


    assertEqual "Liveness 1" (optimize testCode1) expected1
    assertEqual "Liveness 2" (optimize testCode2) expected2
    assertEqual "Liveness 3" (optimize testCode3) expected3

unit_ReadWrite :: IO ()
unit_ReadWrite = do
    let testCode1 = "def f() { read x }"
    let expected1 = [FunctionDeclaration "f" (Function [] [Read "x"] Nothing)]

    let testCode2 = "def f() { x := 0; write x }"
    let expected2 = [FunctionDeclaration "f" (Function [] [Let "x" $ Const 0, Write $ VariableName "x"] Nothing)]

    assertEqual "Liveness 1" (optimize testCode1) expected1
    assertEqual "Liveness 2" (optimize testCode2) expected2

-- unit_If :: IO ()
-- unit_If = do
--     let increment var expr = Let var (Application Addition (VariableName var) expr)

--     let testCode1 = "def f() { y := 0; z := 0; if 0 == 0 then y := y + 3 else z := z + 1 } return y"
--     let expected1 = [FunctionDeclaration "f" (Function [] [Let "y" (Const 0), If (Application Equals (Const 0) (Const 0)) [increment "y" (Const 3)] []] (Just $ VariableName "y"))]
    
--     let testCode2 = "def f() { y := 0; z := 0; if x == 0 then y := y + 3 else z := z + 1 } return y + z"
--     let expected2 = fromJust $ parse testCode2
--     -- [FunctionDeclaration "f" (Function [] [Let "y" (Const 0), Let "z" (Const 0), If (Application Equals (Const 0) (Const 0)) [increment "y" (Const 3)] [increment "z" (Const 1)]] (Just $ Application Addition (VariableName "y") (VariableName "z")))]

--     assertEqual "remove branch" (optimize testCode1) expected1
--     assertEqual "unchanged"     (optimize testCode2) expected2
