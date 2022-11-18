{-# LANGUAGE PatternSynonyms #-}
module Execute where
import Statement (Statement(Write, Read))
import Context ( Context(error), pattern ErrorContext, setVar, setError )
import Error (RuntimeError(UnsupportedError, InvalidInputError))
import Evaluate ( evaluate )
import Control.Monad (foldM)
import Text.Read (readMaybe)


executeStatement :: Context -> Statement -> IO Context
executeStatement c@ErrorContext _ = pure c

executeStatement cxt (Write expr) = do
    let (cxt', x) = evaluate cxt expr
    case x of
        Nothing -> cxt'
        Just res -> print res >> cxt'

executeStatement cxt (Read name) = do
    line <- getLine
    let val = readMaybe line :: Maybe Int
    case val of
        Nothing -> setError cxt $ InvalidInputError line
        Just x -> setVar cxt name x

executeStatement cxt _ = setError cxt UnsupportedError -- TODO

execute :: Context -> [Statement] -> IO Context
execute = foldM executeStatement

run :: Context -> [Statement] -> IO ()
run cxt sts = do
    res <- execute cxt sts
    case Context.error res of
        Nothing -> putStrLn "Success!"
        Just err -> putStrLn $ "Error: " ++ show err
