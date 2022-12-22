{-# LANGUAGE LambdaCase #-}
module Test.Execute where

import Test.Tasty.HUnit (assertEqual, assertBool)
import Statement (Expression(..), Statement (..))
import qualified Data.Map as Map
import Context (Context(..), setVar, setError, VarContext (..), Buffer(..), newContext)
import Evaluate (evaluateStatements)
import Control.Monad.State ( evalStateT, execStateT )
import Error (RuntimeError(..))
import qualified Data.Map as Map
import GHC.IO.Handle (hClose)
import GHC.IO.Handle.FD (stdin)

checkOutput :: Context -> [String] -> Bool
checkOutput cxt out = Context.output cxt == Buffer out

checkError :: Context -> RuntimeError -> Bool
checkError cxt err = Context.error cxt == Just err

unit_executeWrite :: IO ()
unit_executeWrite = do
   let writeConst = Write (Const 1)
   let writeVar = Write (VariableName "var")
   let contextWithVar = newContext { vars = [VarContext (Map.fromList [("var", 123)])] } 

   hClose stdin

   exitContext <- execStateT (evaluateStatements [writeConst]) newContext
   assertBool "write const" $ checkOutput exitContext ["1"]

   exitContext <- execStateT (evaluateStatements [writeVar]) contextWithVar
   assertBool "write var" $ checkOutput exitContext ["123"]

   exitContext <- execStateT (evaluateStatements [writeVar]) newContext
   assertBool "write var failure" $ checkOutput exitContext []
   assertBool "write var failure" $ checkError exitContext (VarNotFound "var")

unit_executeRead :: IO ()
unit_executeRead = do
   let readVar = Read "var"
   let writeConst = Write (Const 1)
   let writeVar = Write (VariableName "var")
   let contextWithInput = newContext { input = Buffer ["123"]}

   hClose stdin

   exitContext <- execStateT (evaluateStatements [readVar, writeVar]) contextWithInput
   assertBool "read var success" $ checkOutput exitContext ["123"]

   exitContext <- execStateT (evaluateStatements [readVar]) newContext
   assertBool "read var failure: end of input" $ checkError exitContext UnexpectedEOF

