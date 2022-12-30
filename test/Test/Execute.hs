module Test.Execute where

import Context (Buffer (..), Context (..), VarContext (..), newContext)
import Control.Monad.State (execStateT)
import qualified Data.Map as Map
import Error (RuntimeError (..))
import Evaluate (evaluateStatements)
import GHC.IO.Handle (hClose)
import GHC.IO.Handle.FD (stdin)
import Statement (Expression (..), Operations (..), Statement (..))
import Test.Tasty.HUnit (assertBool, Assertion, testCase)
import Test.Tasty
import Execute(run)

checkOutput :: Context -> [String] -> Bool
checkOutput cxt out = Context.output cxt == Buffer out

checkError :: Context -> RuntimeError -> Bool
checkError cxt err = Context.error cxt == Just err

noFlushContext :: Context
noFlushContext = newContext {flushEnabled = False}

unit_executeWrite :: Assertion
unit_executeWrite = do
  let writeConst = Write (Const 1)
  let writeVar = Write (VariableName "var")
  let contextWithVar = noFlushContext {vars = [VarContext (Map.fromList [("var", 123)])]}

  hClose stdin

  exitContext <- execStateT (evaluateStatements [writeConst]) noFlushContext
  assertBool "write const" $ checkOutput exitContext ["1"]

  exitContext <- execStateT (evaluateStatements [writeVar]) contextWithVar
  assertBool "write var" $ checkOutput exitContext ["123"]

  exitContext <- execStateT (evaluateStatements [writeVar]) noFlushContext
  assertBool "write var failure" $ checkOutput exitContext []
  assertBool "write var failure" $ checkError exitContext (VarNotFound "var")

unit_executeRead :: Assertion
unit_executeRead = do
  let readVar = Read "var"
  let writeConst = Write (Const 1)
  let writeVar = Write (VariableName "var")
  let contextWithInput = noFlushContext {input = Buffer ["123"]}

  hClose stdin

  exitContext <- execStateT (evaluateStatements [readVar, writeVar]) contextWithInput
  assertBool "read var success" $ checkOutput exitContext ["123"]

  exitContext <- execStateT (evaluateStatements [readVar]) noFlushContext
  assertBool "read var failure: end of input" $ checkError exitContext UnexpectedEOF

unit_basicWhileTest :: Assertion
unit_basicWhileTest = do
  -- let code = "x := 1\n" ++ "write x + 10\n" ++ "while x > 0 do write x; x := x - 1" ++ "write x"
  let code =
        [ Let "x" (Const 1),
          Write $ Application Addition (VariableName "x") (Const 10),
          While
            (Application Greater (VariableName "x") (Const 0))
            [Write $ VariableName "x", Let "x" (Application Subtraction (VariableName "x") (Const 1))],
          Write $ VariableName "x"
        ]
  let context = noFlushContext {input = Buffer []}

  hClose stdin
  exitContext <- execStateT (evaluateStatements code) context
  assertBool "test successfull" $ checkOutput exitContext ["11", "1", "0"]

unit_functions :: Assertion
unit_functions = do
  let code = 
        [
          "def f() { write 1 } return 2", 
          "f()", 
          "f()", 
          "write f() + 2", 
          "x := 1", 
          "def g() { write x }", 
          "g()", 
          "def h() { y := x; write y; write x; x := x + 10; write x } return x",
          "write h()",
          "write x",
          "def sum(a, b) { skip } return a + b",
          "write sum(3, 4)"
        ]
  let context = noFlushContext {input = Buffer []}
  
  hClose stdin
  evaluated <- execStateT (run code) context
  assertBool "function declaration works" $ checkOutput evaluated [
    "1", 
    "1", 
    "1", 
    "4",
    "1",
    "1",
    "1",
    "11",
    "11",
    "1",
    "7"
    ]
  
unitTests :: [TestTree]
unitTests = 
  [ testCase "execute write" unit_executeWrite
  , testCase "execute read" unit_executeRead
  , testCase "basic while test" unit_basicWhileTest
  , testCase "functions" unit_functions
  ]