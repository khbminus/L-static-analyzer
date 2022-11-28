module Evaluate (evaluateStatements, evaluateOneStatement, evaluateExpression) where

import Context (Context (..), InputSource (..), getVar, setVar)
import Control.Composition
import Control.Monad.State
import Error (RuntimeError (..))
import Grammar (number)
import Statement (Expression (..), Operations (..), Statement (..))
import Text.Megaparsec (eof, runParser)
import Text.Read (readMaybe)

evaluateExpression :: Expression -> StateT Context IO (Maybe Int)
evaluateExpression (Const x) = return $ Just x
evaluateExpression (VariableName name) = do
  ctx <- get
  case getVar ctx name of
    x@(Just _) -> return x
    Nothing -> do
      put (ctx { Context.error = Just $ VarNotFound name })
      return Nothing
evaluateExpression (FunctionCall name _) = do
  ctx <- get
  put $ ctx { Context.error = Just $ FunctionNotFound name }
  return Nothing
evaluateExpression (Application op') = do
  let (x, y, op) = unpack op'
  x' <- evaluateExpression x
  y' <- evaluateExpression y
  case (x', y') of
    (Just val_x, Just val_y) -> return $ Just $ op val_x val_y
    (_, _) -> return Nothing
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

evaluateOneStatement :: Statement -> StateT Context IO ()
evaluateOneStatement (Let name value) = do
  value' <- evaluateExpression value
  case value' of
    Just val -> modify (setVar name val)
    Nothing -> pure ()
evaluateOneStatement Skip = pure ()
evaluateOneStatement (While expression statements) = do
  value <- evaluateExpression expression
  case value of
    Just val | toBool val -> pure ()
             | otherwise -> evaluateStatements statements
    Nothing -> pure ()
evaluateOneStatement (If expression trueStatements falseStatements) = do
  value <- evaluateExpression expression
  case value of
    Just val | toBool val -> evaluateStatements trueStatements
             | otherwise -> evaluateStatements falseStatements
    Nothing -> pure ()
evaluateOneStatement (FunctionCallStatement _ _) = pure ()
evaluateOneStatement (Write expr) = do
  value <- evaluateExpression expr
  case value of
    Just val -> lift $ print val
    Nothing -> pure ()
  
evaluateOneStatement (Read var) = do
  ctx <- get
  inp <- lift getLine
  case readMaybe inp :: Maybe Int of
    Nothing -> put $ ctx { Context.error = Nothing }
    Just val -> put $ setVar var val ctx

evaluateStatements :: [Statement] -> StateT Context IO ()
evaluateStatements [] = pure ()
evaluateStatements (x : xs) = do
  evaluateOneStatement x
  evaluateStatements xs
