module Evaluate where
import Statement (Expression (Const, FunctionCall, VariableName, Application))
import Context ( Context(vars), getVar )
import Error ( RuntimeError(UnsupportedError) )


evaluate :: Context -> Expression -> (IO Context, Either RuntimeError Int)
evaluate cxt (Const x) = (pure cxt, Right x)
evaluate cxt (VariableName var) = (pure cxt, getVar (vars cxt) var)
evaluate cxt (FunctionCall _ _) = (pure cxt, Left UnsupportedError) -- TODO
evaluate cxt (Application _) = (pure cxt, Left UnsupportedError)    -- TODO
