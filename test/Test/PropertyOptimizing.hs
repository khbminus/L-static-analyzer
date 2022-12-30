{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.PropertyOptimizing where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog
import Test.PropertyExpr (genExpr)
import qualified Statement as A
import Data.List (nub)

genVarName :: [String] -> Int -> Gen String
genVarName _ 0 = error "too short"
genVarName vars x = Gen.choice generators
    where
        genNew = do
            firstLetter <- Gen.alpha
            suffix <- Gen.string (Range.singleton $ x - 1) Gen.alphaNum
            return $ firstLetter : suffix
        genExisting = Gen.element vars
        generators = genNew : ([genExisting | not (null vars)])


genLet :: [String] -> Int -> Int -> Gen A.Statement
genLet vars len limVal = A.Let <$> genVarName vars len <*> genExpr limVal vars


genStatement :: [String] -> Int -> Int -> Int -> Gen (A.Statement, [String])
genStatement vars bodyLen lenVar limVal = Gen.choice [genLet', genWrite, genIf, genSkip]
    where
        genLet' :: Gen (A.Statement, [String])
        genLet' = do
            l <- genLet vars lenVar limVal
            let (A.Let name _) = l
            return (l, nub $ name : vars)

        genFunCall :: Gen (A.Statement, [String])
        genFunCall = error "unsupported" -- due to my laziness

        genWrite :: Gen (A.Statement, [String])
        genWrite = do
            expr <- genExpr limVal vars
            return (A.Write expr, vars)

        genRead :: Gen (A.Statement, [String])
        genRead = do
            name <- genVarName vars lenVar
            return (A.Read name, nub $ name : vars)

        genWhile :: Gen (A.Statement, [String]) -- generates a lot of infinity loops
        genWhile = do
            expr <- genExpr limVal vars
            (body, vars') <- Gen.subterm (genStatements vars bodyLen lenVar limVal) id
            return (A.While expr body, nub (vars' ++ vars))

        genIf :: Gen (A.Statement, [String])
        genIf = do
            expr <- genExpr limVal vars
            (t, vars') <- Gen.subterm (genStatements vars bodyLen lenVar limVal) id
            let vars'' = nub (vars' ++ vars)
            (f, vars''') <- Gen.subterm (genStatements vars bodyLen lenVar limVal) id
            return (A.If expr t f, nub (vars''' ++ vars''))

        genSkip :: Gen (A.Statement, [String])
        genSkip = return (A.Skip, vars)

genStatements :: [String] -> Int -> Int -> Int -> Gen ([A.Statement], [String])
genStatements vars bodyLen lenVar limVal = do
    len <- Gen.int $ Range.constant 0 bodyLen
    helper len vars bodyLen lenVar limVal
    where
        helper :: Int -> [String] -> Int -> Int -> Int -> Gen ([A.Statement], [String])
        helper 0 _ _ _ _ = return ([], [])
        helper x vars bodyLen lenVar limVal = do
            (prefix, vars') <- helper (x - 1) vars bodyLen lenVar limVal
            let vars'' = nub (vars ++ vars')
            (s, vars''') <- genStatement vars'' bodyLen lenVar limVal
            return (prefix ++ [s], nub (vars''' ++ vars''))

genFunction :: Int -> Int -> Int -> Int -> Gen A.Function -- all functions are void due to my laziness
genFunction argsNum bodyLen lenVar limVal = do
    args <- Gen.subterm (Gen.list (Range.constant 0 argsNum) (genVarName [] lenVar)) id
    (body, _) <- Gen.subterm (genStatements args bodyLen lenVar limVal) id 
    return $ A.Function args body Nothing

genAst :: Int -> Int -> Int -> Int -> Int -> Gen [A.Statement]
genAst numOfFun argsNum bodyLen lenVar limVal = Gen.list (Range.constant 1 numOfFun) genFunDecl
    where
        genFunDecl = A.FunctionDeclaration <$> genVarName [] lenVar <*> genFunction argsNum bodyLen lenVar limVal