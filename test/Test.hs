import Test.Tasty

import qualified Test.PropertyExpr
import qualified Test.Parsers
import qualified Test.Execute
import qualified Test.Evaluate
import qualified Test.ConsoleParser

main :: IO ()
main = defaultMain (testGroup "All Tests"
                    [ testGroup "Property expr" Test.PropertyExpr.props
                    , testGroup "Parsers" Test.Parsers.unitTests
                    , testGroup "Execute" Test.Execute.unitTests
                    , testGroup "Evaluate" Test.Evaluate.unitTests
                    , testGroup "Console parser" Test.ConsoleParser.unitTests 
                    ]) 