{-# LANGUAGE PatternSynonyms #-}
module Context where
import Error ( RuntimeError(VarNameError) )
import qualified Data.Map as Map

data FunContext = FunContext deriving (Show, Eq)

data VarContext = VarContext { context :: Map.Map String Int } deriving (Show, Eq)

getVar :: VarContext -> String -> Either RuntimeError Int
getVar cxt var = let x = Map.lookup var (context cxt) in
    case x of
        Nothing -> Left (VarNameError var)
        Just res -> Right res

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
