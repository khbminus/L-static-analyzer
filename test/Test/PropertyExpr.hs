{-# LANGUAGE BangPatterns #-}
module Test.PropertyExpr where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import Statement
import Evaluate (evaluateExpression)
import Context (newContext)
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog

genOp :: Gen Operations
genOp = Gen.element [Addition, Subtraction, Multiplication, Division, Modulo, Less, Greater, LessOrEquals, GreaterOrEquals, Equals, NotEquals]

genExpr :: Int -> [String] -> Gen Expression
genExpr n vars =
    Gen.recursive
        Gen.choice
        nonRecGens
        [
            binOpGen
        ]
    where
        binOpGen = do
            op <- genOp
            Gen.subterm2 (genExpr n vars) (genExpr n vars) (Application op)
        numGen = Const <$> Gen.int (Range.constant 0 n)
        varGen = VariableName <$> Gen.element vars

        nonRecGens = numGen : ([varGen | not (null vars)])


evalExpr :: Expression -> Maybe Int
evalExpr (Const x) = Just x
evalExpr (Application op x y) = do
    x' <- evalExpr x
    y' <- evalExpr y
    getF op x' y'
    where
        getF :: Operations -> (Int -> Int -> Maybe Int)
        getF Addition = \a b -> Just $ a + b
        getF Subtraction = \a b -> Just $ a - b
        getF Multiplication = \a b -> Just $ a * b
        getF Modulo = \a b -> if b == 0 then Nothing else Just $ a `mod` b
        getF Division = \a b -> if b == 0 then Nothing else Just $ a `div` b
        getF Less = \a b -> Just $ if a < b then 1 else 0
        getF Greater = \a b -> Just $ if a > b then 1 else 0
        getF LessOrEquals = \a b -> Just $ if a <= b then 1 else 0
        getF GreaterOrEquals = \a b -> Just $ if a >= b then 1 else 0
        getF Equals = \a b -> Just $ if a == b then 1 else 0
        getF NotEquals = \a b -> Just $ if a /= b then 1 else 0
        getF _ = error "operation not supported"
evalExpr _ = error "expression not supported"

prop_eval_correct :: Property
prop_eval_correct = property $ do
    expr <- forAll $ genExpr 1000 []
    let !res = evalExpr expr
    let ctx = newContext
    let a = evalStateT (runMaybeT (evaluateExpression expr)) ctx
    res' <- liftIO a
    res === res'

props :: [TestTree]
props =
    [ testProperty "Test correctness of expression evaluation" prop_eval_correct ]