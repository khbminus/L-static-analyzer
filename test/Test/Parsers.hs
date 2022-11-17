module Test.Parsers where

import Grammar
import Statement
import Test.HUnit
import Text.Megaparsec

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

  assertBool "bad keywords are banned" $ failVar "while"
  assertBool "bad keywords are banned" $ failVar "do"
  assertBool "bad keywords are banned" $ failVar "if"
  assertBool "bad keywords are banned" $ failVar "then"
  assertBool "bad keywords are banned" $ failVar "else"

unit_parser_expr :: IO ()
unit_parser_expr = do
  let succExpr = parseSuccessful expression
  let failExpr = parseFailed expression

  assertBool "simple expression" $ succExpr "1" (Const 1)
  assertBool "simple with parens" $ succExpr "(1)" (Const 1)
  assertBool "operations works fine" $ succExpr "1 + 3" (Application $ Addition (Const 1) (Const 3))
  assertBool "precedence works fine" $
    succExpr
      "1 * 2 + 3"
      ( Application $
          Addition
            ( Application $
                Multiplication
                  (Const 1)
                  (Const 2)
            )
            (Const 3)
      )
  
  
