{-# LANGUAGE PatternSynonyms #-}
module Execute where
import Statement (Statement(Write))
import Context ( Context(error), pattern ErrorContext )
import Error (RuntimeError(UnsupportedError))
import Evaluate ( evaluate )
import Control.Monad (foldM)


executeStatement :: Context -> Statement -> IO Context
executeStatement c@ErrorContext _ = pure c
executeStatement cxt (Write expr) = do
    let (cxt', x) = evaluate cxt expr
    putStrLn (case x of
        Left err -> show err
        Right res -> show res)
    cxt'
executeStatement cxt _ = pure $ cxt { Context.error = Just UnsupportedError }

execute :: Context -> [Statement] -> IO Context
execute = foldM executeStatement
