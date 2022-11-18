module Execute where
import Statement (Statement(Write))
import Context (Context(io, error))
import Error (RuntimeError(UnsupportedError))
import Evaluate ( evaluate )


executeStatement :: Context -> Statement -> Context
executeStatement cxt (Write expr) = cxt' { io = io cxt' >> putStrLn unwrap_x } where
    (cxt', x) = evaluate cxt expr
    unwrap_x = case x of
        Left err -> show err
        Right res -> show res
executeStatement cxt _ = cxt { Context.error = Just UnsupportedError }