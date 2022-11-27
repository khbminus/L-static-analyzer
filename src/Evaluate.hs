{-# LANGUAGE BangPatterns #-}

module Evaluate (evaluateStatements, evaluateOneStatement, evaluateExpression) where

import Context (Context (..), InputSource (..), getVar, setVar)
import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe)
import Error (RuntimeError (UnsupportedError))
import Statement (Expression (..), Operations (..), Statement (..))

evaluateExpression :: Expression -> Context -> Int
evaluateExpression (Const x) _ = x
evaluateExpression (VariableName name) ctx = fromMaybe undefined (getVar ctx name)
evaluateExpression (FunctionCall _ _) _ = undefined
evaluateExpression (Application op) ctx = evaluateOp op ctx
  where
    evaluateOp :: Operations -> Context -> Int
    evaluateOp (Addition lft rgt) ctx = evaluateExpression lft ctx + evaluateExpression rgt ctx
    evaluateOp (Subtraction lft rgt) ctx = evaluateExpression lft ctx - evaluateExpression rgt ctx
    evaluateOp (Division lft rgt) ctx = evaluateExpression lft ctx `div` evaluateExpression rgt ctx
    evaluateOp (Multiplication lft rgt) ctx = evaluateExpression lft ctx * evaluateExpression rgt ctx
    evaluateOp (Modulo lft rgt) ctx = evaluateExpression lft ctx `mod` evaluateExpression rgt ctx
    evaluateOp (Equals lft rgt) ctx = fromBool $ evaluateExpression lft ctx == evaluateExpression rgt ctx
    evaluateOp (NotEquals lft rgt) ctx = fromBool $ evaluateExpression lft ctx /= evaluateExpression rgt ctx
    evaluateOp (Greater lft rgt) ctx = fromBool $ evaluateExpression lft ctx > evaluateExpression rgt ctx
    evaluateOp (GreaterOrEquals lft rgt) ctx = fromBool $ evaluateExpression lft ctx >= evaluateExpression rgt ctx
    evaluateOp (Less lft rgt) ctx = fromBool $ evaluateExpression lft ctx < evaluateExpression rgt ctx
    evaluateOp (LessOrEquals lft rgt) ctx = fromBool $ evaluateExpression lft ctx <= evaluateExpression rgt ctx
    evaluateOp (LazyAnd lft rgt) ctx = case evaluateExpression lft ctx of
      0 -> 0
      _ -> boolToInt $ evaluateExpression rgt ctx
    evaluateOp (LazyOr lft rgt) ctx = case evaluateExpression lft ctx of
      0 -> boolToInt $ evaluateExpression rgt ctx
      _ -> 1

    fromBool :: Bool -> Int
    fromBool True = 1
    fromBool False = 0

    boolToInt :: Int -> Int
    boolToInt 0 = 0
    boolToInt _ = 1

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

evaluateOneStatement :: Statement -> State Context ()
evaluateOneStatement (Let name value) = do
  ctx <- get
  let !value' = evaluateExpression value ctx
  put $ setVar ctx name value'
evaluateOneStatement Skip = pure ()
evaluateOneStatement (While expression statements) = do
  ctx <- get
  let !value = evaluateExpression expression ctx
  if toBool value
    then return ()
    else evaluateStatements statements
evaluateOneStatement (If expression trueStatements falseStatements) = do
  ctx <- get
  let !value = evaluateExpression expression ctx
  if toBool value
    then evaluateStatements trueStatements
    else evaluateStatements falseStatements
evaluateOneStatement (FunctionCallStatement name args) = pure ()
evaluateOneStatement (Write expr) = do
  ctx <- get
  let !value = evaluateExpression expr ctx
  put ctx {output = output ctx ++ [show value]}
evaluateOneStatement (Read val) = do
  ctx <- get
  let value = 0 -- TODO: make it works
  put (setVar ctx val value) {input = (input ctx) {inputLines = tail $ inputLines $ input ctx}}

evaluateStatements :: [Statement] -> State Context ()
evaluateStatements [] = pure ()
evaluateStatements (x : xs) = do
  evaluateOneStatement x
  evaluateStatements xs
