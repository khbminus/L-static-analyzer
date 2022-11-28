module Evaluate (evaluateStatements, evaluateOneStatement, evaluateExpression) where

import Context (Context (..), InputSource (..), getVar, setVar)
import Control.Composition
import Control.Monad.State
import Error (RuntimeError (..))
import Grammar (number)
import Statement (Expression (..), Operations (..), Statement (..))
import Text.Megaparsec (eof, runParser)

evaluateExpression :: Expression -> StateT Context (Either RuntimeError) Int
evaluateExpression (Const x) = return x
evaluateExpression (VariableName name) = do
  ctx <- get
  case getVar ctx name of
    Just x -> return x
    _ -> lift $ Left $ VarNotFound name
evaluateExpression (FunctionCall name _) = lift $ Left $ FunctionNotFound name
evaluateExpression (Application op') = do
  let (x, y, op) = unpack op'
  x' <- evaluateExpression x
  y' <- evaluateExpression y
  return $ op x' y'
  where
    -- FIXME: fix that crappy design
    unpack :: Operations -> (Expression, Expression, Int -> Int -> Int)
    unpack (Addition lft rgt) = (lft, rgt, (+))
    unpack (Subtraction lft rgt) = (lft, rgt, (-))
    unpack (Division lft rgt) = (lft, rgt, div)
    unpack (Multiplication lft rgt) = (lft, rgt, (*))
    unpack (Modulo lft rgt) = (lft, rgt, mod)
    unpack (Equals lft rgt) = (lft, rgt, fromBool .* (==))
    unpack (NotEquals lft rgt) = (lft, rgt, fromBool .* (/=))
    unpack (Greater lft rgt) = (lft, rgt, fromBool .* (>))
    unpack (GreaterOrEquals lft rgt) = (lft, rgt, fromBool .* (>=))
    unpack (Less lft rgt) = (lft, rgt, fromBool .* (<))
    unpack (LessOrEquals lft rgt) = (lft, rgt, fromBool .* (<=))
    unpack (LazyAnd lft rgt) = (lft, rgt, lazyAnd)
    unpack (LazyOr lft rgt) = (lft, rgt, lazyOr)

    lazyAnd :: Int -> Int -> Int
    lazyAnd lft rgt = if lft == 0 then 0 else boolToInt rgt

    lazyOr :: Int -> Int -> Int
    lazyOr lft rgt = if lft /= 0 then 1 else boolToInt rgt

    fromBool :: Bool -> Int
    fromBool True = 1
    fromBool False = 0

    boolToInt :: Int -> Int
    boolToInt 0 = 0
    boolToInt _ = 1

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

evaluateOneStatement :: Statement -> StateT Context (Either RuntimeError) ()
evaluateOneStatement (Let name value) = do
  value' <- evaluateExpression value
  modify (setVar name value')
evaluateOneStatement Skip = pure ()
evaluateOneStatement (While expression statements) = do
  value <- evaluateExpression expression
  if not $ toBool value
    then return ()
    else do 
      evaluateStatements statements
      evaluateOneStatement (While expression statements)
evaluateOneStatement (If expression trueStatements falseStatements) = do
  value <- evaluateExpression expression
  if toBool value
    then evaluateStatements trueStatements
    else evaluateStatements falseStatements
evaluateOneStatement (FunctionCallStatement name args) = pure ()
evaluateOneStatement (Write expr) = do
  value <- evaluateExpression expr
  ctx <- get
  put $ ctx {output = output ctx ++ [show value]}
evaluateOneStatement (Read val) = do
  ctx <- get
  case (inputLines . input) ctx of
    [] -> lift $ Left UnexpectedEOF
    (x : xs) -> do
      case runParser (number <* eof) (fileName $ input ctx) x of
        Left e -> lift $ Left $ ParserError e
        Right value -> put (setVar val value ctx) {input = (input ctx) {inputLines = xs}}

evaluateStatements :: [Statement] -> StateT Context (Either RuntimeError) ()
evaluateStatements [] = pure ()
evaluateStatements (x : xs) = do
  evaluateOneStatement x
  evaluateStatements xs
