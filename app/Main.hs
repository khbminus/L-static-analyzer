module Main (main) where

import Statement(Statement(Write, Skip), Expression(Const))
import Execute (execute)
import Context (emptyContext, error)

main :: IO ()
main = do
    let st = Write (Const 1)
    let err = Skip
    res <- execute emptyContext [st, st, st]
    case Context.error res of
        Nothing -> putStrLn "Success!"
        Just err -> putStrLn $ "Error: " ++ show err

    res <- execute emptyContext [st, err, st]
    case Context.error res of
        Nothing -> putStrLn "Success!"
        Just err -> putStrLn $ "Error: " ++ show err
