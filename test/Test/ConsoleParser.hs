module Test.ConsoleParser where

import Grammar (Parser)
import ConsoleParser (varArgParser, getVarContext)
import Test.Tasty.HUnit
import Test.Tasty
import Text.Megaparsec
import Context (VarContext(..))
import qualified Data.Map as Map

parseSuccessful :: Eq a => Parser a -> String -> a -> Bool
parseSuccessful parser line result = case parse (parser <* eof) "" line of
  Left _ -> False
  Right a -> a == result

parseFailed :: Parser a -> String -> Bool
parseFailed parser line = case parse (parser <* eof) "" line of
  Left _ -> True
  Right _ -> False

unit_varArgParser :: IO ()
unit_varArgParser = do
  let success = parseSuccessful varArgParser
  let failure = parseFailed varArgParser

  assertBool "1" $ success "x=10" ("x", 10)
  assertBool "3" $ success "x=0" ("x", 0)

  assertBool "4" $ failure "x tr=1"
  assertBool "5" $ failure "1tr=5"
  assertBool "6" $ failure "x=vr"

varContextComp :: [String] -> [(String, Int)] -> Bool
varContextComp inp cxt = getVarContext inp == VarContext { varContext = Map.fromList cxt }

unit_getVarContext :: Assertion
unit_getVarContext = do
  assertBool "1" $ varContextComp ["var=234", "var2=0"] [("var", 234), ("var2", 0)]
  assertBool "2" $ varContextComp ["var=24", "var=0"]   [("var", 24)]

unitTests :: [TestTree]
unitTests = 
  [ testCase "var arg parser" unit_varArgParser
  , testCase "get var context" unit_getVarContext
  ]