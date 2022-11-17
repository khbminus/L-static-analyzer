module Test.Parsers where

import Text.Megaparsec
import Grammar
import Statement

import Test.HUnit

parseSuccessful :: Eq a => Parser a -> String -> a -> Bool
parseSuccessful parser line result = case parse parser "" line of
  Left _ -> False
  Right a -> a == result

parseFailed :: Parser a -> String -> Bool
parseFailed parser line = case parse parser "" line of
  Left _ -> True
  Right _ -> False


unit_parser_const :: IO ()
unit_parser_const = do
  let succConst = parseSuccessful pConst
  let failConst = parseFailed pConst

  assertBool "const parser failed" $ succConst "1" (Const 1)
  assertBool "const parser failed" $ succConst "1.23456" (Const 1)
  assertBool "const parser failed" $ succConst "1234567" (Const 1234567)
  assertBool "const parser failed" $ failConst "ahahahahh1234"

unit_parser_var_name :: IO ()
unit_parser_var_name = do
  let succVar = parseSuccessful pVarName
  let failVar = parseFailed pVarName

  assertBool "var parser failed" $ failVar "1234abc"
  assertBool "var parser failed" $ failVar ""
  assertBool "var parser failed" $ succVar "abcd" (VariableName "abcd")
  assertBool "var parser failed" $ succVar "a1234" (VariableName "a1234")


