module Evaluate where
import Statement (Expression (Const, FunctionCall, VariableName, Application))
import Context ( setError, Context(), getVar )
import Error ( RuntimeError(UnsupportedError) )


evaluate :: Context -> Expression -> (IO Context, Maybe Int)
evaluate cxt (Const x) = (pure cxt, Just x)
evaluate cxt (VariableName var) = getVar cxt var

evaluate cxt (FunctionCall _ _) = (setError cxt UnsupportedError, Nothing) -- TODO
evaluate cxt (Application _) = (setError cxt UnsupportedError, Nothing)    -- TODO
