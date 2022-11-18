module Main (main) where

import Statement(Statement(Write), Expression(Const))
import Execute (executeStatement)
import GHC.Base (IO(IO))
import Context ( Context(Context, io), FunContext(FunContext), emptyContext)

main :: IO ()
main = do
    let st = Write (Const 1)
    io $ executeStatement emptyContext st
