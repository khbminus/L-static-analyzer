{-# LANGUAGE PatternSynonyms #-}
module Context where
import Error ( RuntimeError(VarNameError) )
import qualified Data.Map as Map

data FunContext = FunContext deriving (Show, Eq)

data VarContext = VarContext { context :: Map.Map String Int } deriving (Show, Eq)

emptyVarContext :: VarContext
emptyVarContext = VarContext { context = Map.empty }

data Context = Context { funs :: FunContext, vars :: VarContext, error :: Maybe RuntimeError }

emptyContext :: Context
emptyContext = Context {
    funs = FunContext,
    vars = emptyVarContext,
    Context.error = Nothing
}

pattern ErrorContext :: Context
pattern ErrorContext <- Context { Context.error = (Just _) }

getVar :: Context -> String -> (IO Context, Maybe Int)
getVar cxt var = 
    let mp = context . vars $ cxt in
    let x = Map.lookup var mp in
    (case x of
        Nothing -> setError cxt $ VarNameError var
        Just _ -> pure cxt
    , x)

setVar :: Context -> String -> Int -> IO Context
setVar cxt name val = 
    let mp = context . vars $ cxt in
    pure $ cxt { vars = VarContext $ Map.insert name val mp }

setError :: Context -> RuntimeError -> IO Context
setError cxt err = pure $ cxt { Context.error = Just err }
