module Test.Parsers where

import Grammar
import Statement
import Test.HUnit
import Text.Megaparsec
import Grammar (REPLInput(..))

parseSuccessful :: Eq a => Parser a -> String -> a -> Bool
parseSuccessful parser line result = case parse (parser <* eof) "" line of
  Left _ -> False
  Right a -> a == result

parseFailed :: Parser a -> String -> Bool
parseFailed parser line = case parse (parser <* eof) "" line of
  Left _ -> True
  Right _ -> False

unit_const :: IO ()
unit_const = do
  let succConst = parseSuccessful constValue
  let failConst = parseFailed constValue

  assertBool "const parser failed" $ succConst "1" (Const 1)
  assertBool "const parser failed" $ failConst "1.23456"
  assertBool "const parser failed" $ succConst "1234567" (Const 1234567)
  assertBool "const parser failed" $ failConst "ahahahahh1234"

unit_var_name :: IO ()
unit_var_name = do
  let succVar = parseSuccessful varName
  let failVar = parseFailed varName

  assertBool "var parser failed" $ failVar "1234abc"
  assertBool "var parser failed" $ failVar ""
  assertBool "var parser failed" $ succVar "abcd" (VariableName "abcd")
  assertBool "var parser failed" $ succVar "a1234" (VariableName "a1234")

  assertBool "bad keywords are banned" $ failVar "while"
  assertBool "bad keywords are banned" $ failVar "do"
  assertBool "bad keywords are banned" $ failVar "if"
  assertBool "bad keywords are banned" $ failVar "then"
  assertBool "bad keywords are banned" $ failVar "else"

unit_expr :: IO ()
unit_expr = do
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

  assertBool "fails on unary" $ failExpr "+1"
  assertBool "fails on bad expr" $ failExpr "1+2++-"

unit_let :: IO ()
unit_let = do
  let success = parseSuccessful letVariable
  let fail = parseFailed letVariable

  assertBool "to const" $ success "x := 1" [Let "x" (Const 1)]
  assertBool "reassign" $ success "x := x" [Let "x" (VariableName "x")]
  assertBool "compicated expression" $
    success
      "x := y % 4 + 2 * 3"
      [ Let
          "x"
          ( Application $
              Addition
                ( Application $
                    Modulo
                      (VariableName "y")
                      (Const 4)
                )
                ( Application $
                    Multiplication
                      (Const 2)
                      (Const 3)
                )
          )
      ]

  assertBool "assign statement" $ fail "x := while 1 do 2"

  assertBool "assign function call" $
    success
      "loooooong := function(first, second, third, 1, 2 + 3)"
      [ Let
          "loooooong"
          ( FunctionCall
              "function"
              [ VariableName "first",
                VariableName "second",
                VariableName "third",
                Const 1,
                Application $ Addition (Const 2) (Const 3)
              ]
          )
      ]

unit_while :: IO ()
unit_while = do
  let success = parseSuccessful while
  let fail = parseFailed while

  assertBool "simple while" $ success "while 1 do x := x" [While (Const 1) [Let "x" (VariableName "x")]]
  assertBool "complicated expression" $
    success
      "while 1 + 2 do x := x"
      [ While
          (Application $ Addition (Const 1) (Const 2))
          [Let "x" (VariableName "x")]
      ]

  assertBool "function call" $
    success
      "while f(1) do x := x"
      [ While
          (FunctionCall "f" [Const 1])
          [Let "x" (VariableName "x")]
      ]

  assertBool "just while fails" $ fail "while"
  assertBool "just while-do failes" $ fail "while do"
  assertBool "without statement fail" $ fail "while 1 do"
  assertBool "without condition fail" $ fail "while do x := x"

unit_if :: IO ()
unit_if = do
  let success = parseSuccessful ifThenElse
  let fail = parseFailed ifThenElse

  assertBool "simple if" $
    success
      "if 1 then a(1) else a(2)"
      [ If
          (Const 1)
          [FunctionCallStatement "a" [Const 1]]
          [FunctionCallStatement "a" [Const 2]]
      ]

  assertBool "if fails with statement in condition" $ fail "if x := 1 then a 1 else a 2"

unit_statement :: IO ()
unit_statement = do
  let success = parseSuccessful statement
  let fail = parseFailed statement

  assertBool "function call" $ success "f(1, 2, 3)" [FunctionCallStatement "f" [Const 1, Const 2, Const 3]]
  assertBool "read variable" $ success "read x" [Read "x"]
  assertBool "read expression fails" $ fail "read x + 2"
  assertBool "write variable" $ success "write x" [Write (VariableName "x")]
  assertBool "write complex expression" $
    success
      "write x + 2 * 3"
      [ Write $
          Application $
            Addition
              (VariableName "x")
              ( Application $
                  Multiplication
                    (Const 2)
                    (Const 3)
              )
      ]
  assertBool "skip statement" $ success "skip" [Skip]
  assertBool "multiplie statements" $ success "x := a; y := b" [Let "x" $ VariableName "a", Let "y" $ VariableName "b"]
  assertBool "while with long body" $
    success
      "while 1 do x := a; y := b"
      [ While
          (Const 1)
          [ Let "x" $ VariableName "a",
            Let "y" $ VariableName "b"
          ]
      ]

unit_expressionOrStatement = do
  assertBool "failure"  $ fail "rtwe tre"
  assertBool "variable" $ success "var" (ConsoleExpression $ VariableName "var")

  where 
    success inp expected = case parseStatementOrExpression inp of
      Left _ -> False
      Right res -> res == expected
    fail inp = case parseStatementOrExpression inp of
      Left _ -> True
      Right _ -> False

unit_functionsDeclarations :: IO ()
unit_functionsDeclarations = do
  let success = parseSuccessful functionDeclaration
  let fail = parseFailed functionDeclaration

  assertBool "Simple function without return" $ success "def f() {skip}" [FunctionDeclaration "f" (Function [] [Skip] Nothing)]
  assertBool "Function with empty body" $ fail "def f() {}"
  assertBool "Wierd spaces" $ success "def        f    (   )   {     skip     }" [FunctionDeclaration "f" (Function [] [Skip] Nothing)]
  assertBool "Multiline functions" $ success "def f() \n {\n skip \n}" [FunctionDeclaration "f" (Function [] [Skip] Nothing)]
  assertBool "A lot of statements inside body" $ success "def f() { x := 1; skip  }" [FunctionDeclaration "f" (Function [] [Let "x" (Const 1), Skip] Nothing)]
  assertBool "Long function name" $ success "def ffffffffffffffffffffffffff() { skip }" [FunctionDeclaration "ffffffffffffffffffffffffff" (Function [] [Skip] Nothing)]

  assertBool "without def" $ fail "f() {skip}"
  assertBool "without braces" $ fail "def f { skip }"

  assertBool "With return expression" $ success "def f() { skip } return 2" [FunctionDeclaration "f" (Function [] [Skip] (Just $ Const 2))]
  assertBool "With params" $ success "def f(a, b, c, d, e, f) { skip  }" [FunctionDeclaration "f" (Function ["a", "b", "c", "d", "e", "f"] [Skip] Nothing)]
  assertBool "Identity function" $ success "def f(x) { skip } return x" [FunctionDeclaration "f" (Function ["x"] [Skip] (Just $ VariableName "x"))]
  
  assertBool "Wierd argument name" $ fail "def f(asdas  d  sda  ) {skip}"
  assertBool "Unclosed comma" $ fail "def f(a,) {skip}"
