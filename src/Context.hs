{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
module Context where
import Error ( RuntimeError(VarNameError) )
import qualified Data.Map as Map

data FunContext = FunContext deriving (Show, Eq)

data VarContext = VarContext { context :: Map.Map String Int } deriving (Show, Eq)

emptyVarContext :: VarContext
emptyVarContext = VarContext { context = Map.empty }

setVarContext :: VarContext -> String -> Int -> VarContext
setVarContext cxt var val = VarContext $ Map.insert var val $ context cxt

data Context = Context 
    { funs :: FunContext
    , vars :: VarContext
    , error :: Maybe RuntimeError 
    , getNextLine :: IO String
    , putLine :: String -> IO ()
    }

instance Show Context where
    show :: Context -> String
    show cxt = "Functions: " ++ show (funs cxt) ++ "\nVariables: " ++ show (vars cxt) ++ "\nError: " ++ show (Context.error cxt)

instance Eq Context where
    (==) :: Context -> Context -> Bool
    (==) c1 c2 = funs c1 == funs c2 && vars c1 == vars c2 && Context.error c1 == Context.error c2

emptyContext :: Context
emptyContext = Context 
    { funs = FunContext
    , vars = emptyVarContext
    , Context.error = Nothing
    , getNextLine = getLine
    , putLine = putStrLn
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

setVar :: Context -> String -> Int -> Context
setVar cxt name val = 
    let mp = context . vars $ cxt in
    cxt { vars = VarContext $ Map.insert name val mp }

setError :: Context -> RuntimeError -> IO Context
setError cxt err = pure $ cxt { Context.error = Just err }
