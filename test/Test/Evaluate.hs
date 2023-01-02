module Test.Evaluate where

import Evaluate (evaluateList)
import Context (newContext)
import Test.Tasty.HUnit
import Test.Tasty
import Text.Megaparsec
import Statement (Expression(..))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))

testEvaluateList :: [Expression] -> Maybe [Int] -> IO Bool
testEvaluateList expr res = do
    eval <- evalStateT (runMaybeT $ evaluateList expr) newContext 
    pure $ eval == res

unit_evaluateExprList = do
    let assert msg action = action >>= assertBool msg

    assert "success" $ testEvaluateList [Const 1] (Just [1])
    assert "success" $ testEvaluateList [Const 1, Const 2] (Just [1, 2])
    assert "failure" $ testEvaluateList [VariableName "var"] Nothing
    assert "failure" $ testEvaluateList [Const 1, VariableName "var"] Nothing
    assert "failure" $ testEvaluateList [VariableName "var", Const 2] Nothing

unitTests :: [TestTree]
unitTests = [ testCase "evaluate expr list" unit_evaluateExprList ]