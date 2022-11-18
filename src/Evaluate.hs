module Evaluate where
import Statement (Expression (Const, FunctionCall, VariableName, Application))
import Context ( Context(vars), getVar )
import Error ( RuntimeError(UnsupportedError) )


evaluate :: Context -> Expression -> (Context, Either RuntimeError Int)
evaluate cxt (Const x) = (cxt, Right x)
evaluate cxt (VariableName var) = (cxt, getVar (vars cxt) var)
evaluate cxt (FunctionCall _ _) = (cxt, Left UnsupportedError) -- TODO
evaluate cxt (Application _) = (cxt, Left UnsupportedError)    -- TODO
