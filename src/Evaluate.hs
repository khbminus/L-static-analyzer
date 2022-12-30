module Evaluate (evaluateStatements, evaluateOneStatement, evaluateExpression, evaluateList) where

import Context (Context (..), getFunT, getVarT, loadFunStack, setFun, setVar, unloadFunStack, popInput, setErrorT, pushOutput, flush)
import Control.Composition
import Control.Monad.State
import Statement (Expression (..), Function (..), Operations (..), Statement (..))
import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Data.Maybe (isNothing, fromJust, isJust)
import Error (RuntimeError(InvalidInput, UnexpectedEOF, DivisionByZero))
import GHC.IO.Handle (hIsOpen)
import GHC.IO.Handle.FD (stdin)

evaluateList :: [Expression] -> MaybeT (StateT Context IO) [Int]
evaluateList [] = return []
evaluateList (x : xs) = do
  x' <- evaluateExpression x
  xs' <- evaluateList xs
  return $ x' : xs'


evaluateExpression :: Expression -> MaybeT (StateT Context IO) Int
evaluateExpression (Const x) = return x
evaluateExpression (VariableName name) = getVarT name

evaluateExpression (FunctionCall name argumentValues) = do
  f <- getFunT name
  args <- evaluateList argumentValues
  modify (loadFunStack f args) -- FIXME: check length
  let Function _ statements returnExpr = f
  lift $ evaluateStatements statements
  when (isNothing returnExpr) $ do { modify unloadFunStack; mzero }
  let expr = fromJust returnExpr
  returnValue <- evaluateExpression expr
  modify unloadFunStack
  return returnValue

evaluateExpression (Application op x y) = do
  x' <- evaluateExpression x
  y' <- evaluateExpression y
  case unpack op x' y' of
    Just v -> return v
    Nothing -> do
      ctx <- get
      put ctx {Context.error = Just $ DivisionByZero}
      mzero

  where
    unpack :: Operations -> (Int -> Int -> Maybe Int)
    unpack Addition = Just .* (+)
    unpack Subtraction = Just .* (-)
    unpack Division = \a b -> if b == 0 then Nothing else Just $ a `div` b
    unpack Multiplication = Just .* (*)
    unpack Modulo = \a b -> if b == 0 then Nothing else Just $ a `mod` b
    unpack Equals = Just . fromBool .* (==)
    unpack NotEquals = Just . fromBool .* (/=)
    unpack Greater = Just . fromBool .* (>)
    unpack GreaterOrEquals = Just . fromBool .* (>=)
    unpack Less = Just . fromBool .* (<)
    unpack LessOrEquals = Just . fromBool .* (<=)
    unpack LazyAnd = Just .* lazyAnd
    unpack LazyOr = Just .* lazyOr

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
  value' <- runMaybeT $ evaluateExpression value
  case value' of
    Just val -> modify (setVar name val)
    Nothing -> pure ()

evaluateOneStatement Skip = pure ()

evaluateOneStatement (While expression statements) = do
  value <- runMaybeT $ evaluateExpression expression
  case value of
    Just val
      | toBool val -> evaluateStatements statements
      | otherwise -> pure ()
    Nothing -> pure ()

evaluateOneStatement (If expression trueStatements falseStatements) = do
  value <- runMaybeT $ evaluateExpression expression
  case value of
    Just val
      | toBool val -> evaluateStatements trueStatements
      | otherwise -> evaluateStatements falseStatements
    Nothing -> pure ()

evaluateOneStatement (FunctionCallStatement name argumentValues) =
  void (runMaybeT (evaluateExpression $ FunctionCall name argumentValues))

evaluateOneStatement (Write expr) = do
  value <- runMaybeT $ evaluateExpression expr
  case value of
    Just val -> pushOutput $ show val
    Nothing -> pure ()

evaluateOneStatement (Read var) = do
  cxt <- get
  inp <- runMaybeT popInput
  str <- if isJust inp then pure inp else runMaybeT maybeGetLine
  if isNothing str then return ()
  else let justStr = fromJust str in case readMaybe justStr :: Maybe Int of
    Nothing -> setErrorT $ InvalidInput justStr
    Just val -> put $ setVar var val cxt

  where
    maybeGetLine :: MaybeT (StateT Context IO) String
    maybeGetLine = do
      cond <- liftIO $ hIsOpen stdin
      if cond
      then liftIO getLine
      else do { lift $ setErrorT UnexpectedEOF; mzero }

evaluateOneStatement (FunctionDeclaration name f) = do
  modify $ setFun name f

evaluateStatements :: [Statement] -> StateT Context IO ()
evaluateStatements [] = pure ()
evaluateStatements (x : xs) = do
  evaluateOneStatement x
  cxt <- get
  when (flushEnabled cxt) flush
  evaluateStatements xs
