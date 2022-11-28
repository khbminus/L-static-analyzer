module Test.Execute where

import Context (Context (..), InputSource (..), newContext)
import Control.Monad.Trans.State
import Error (RuntimeError (..))
import Execute (run)
import Test.HUnit

--          code        input       input       output
testRun :: [String] -> [String] -> Either RuntimeError ([String], [String])
testRun instructions inpLines =
  let inputSource = InputSource {fileName = "testInput", inputLines = inpLines}
   in let e = runStateT (run "testCode" instructions) $ newContext inputSource
       in case e of
            Left e' -> Left e'
            Right (_, ctx) -> Right ((inputLines . input) ctx, output ctx)

throws :: [String] -> [String] -> RuntimeError -> Bool
throws instructions inp e = case testRun instructions inp of
  Left e' -> e' == e
  Right _ -> False

parseFailed :: [String] -> [String] -> Bool
parseFailed instructions inp = case testRun instructions inp of
  Left (ParserError _) -> True
  _ -> False

successAndEquals :: [String] -> [String] -> [String] -> Bool
successAndEquals instructions inp out = case testRun instructions inp of
  Left _ -> False
  Right (a, b) -> null a && b == out

unit_simple :: IO ()
unit_simple = do
  assertBool "empty statement" $ successAndEquals [] [] []
  assertBool "just skip" $ successAndEquals ["skip"] [] []
  assertBool "invalid text" $ parseFailed ["sadsadsadsa"] []

unit_write :: IO ()
unit_write = do
  assertBool "write const" $ successAndEquals ["write 1"] [] ["1"]
  assertBool "write easy expr" $ successAndEquals ["write 1+2"] [] ["3"]
  assertBool "write var" $ throws ["write aboba"] [] $ VarNotFound "aboba"

unit_lets :: IO ()
unit_lets = do
  assertBool "let const" $ successAndEquals ["x := 1", "write x"] [] ["1"]
  assertBool "let expr" $ successAndEquals ["x := 1 + 2 * 3", "write x"] [] ["7"]
  assertBool "self assign" $ successAndEquals ["x := 1", "write x", "x := x", "write x"] [] ["1", "1"]
  assertBool "self arithmetics" $ successAndEquals ["x := 1", "x := x + 1", "write x"] [] ["2"]
  assertBool "multiplie variables" $
    successAndEquals
      ["x := 1; y := 2", "z := x + y", "write x", "write y", "write z"]
      []
      ["1", "2", "3"]

unit_read :: IO ()
unit_read = do
  assertBool "simple read" $ successAndEquals ["read x", "write x"] ["1"] ["1"]
  assertBool "two reads" $ successAndEquals ["read x", "read y", "write x", "write y"] ["1", "2"] ["1", "2"]
  assertBool "unexcepted EOF" $ throws ["read x"] [] UnexpectedEOF
  assertBool "overlapping" $ successAndEquals ["x := 1", "write x", "read x", "write x"] ["239"] ["1", "239"]
  assertBool "invalid input" $ parseFailed ["read x"] ["aboba"]

unit_while :: IO ()
unit_while = do
  assertBool "simple while" $ successAndEquals ["while 0 do write 1"] [] []
  assertBool "for cycle" $ successAndEquals 
    ["x := 5", "while x do write x; x := x - 1"] 
    [] 
    ["5", "4", "3", "2", "1"]
