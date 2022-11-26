{-# LANGUAGE InstanceSigs #-}
module Context where
import qualified Data.Map as Map

data FunContext = FunContext deriving (Show, Eq)

data VarContext = VarContext { context :: Map.Map String Int } deriving (Show, Eq)

emptyVarContext :: VarContext
emptyVarContext = VarContext { context = Map.empty }

data Context = Context 
    { funs :: FunContext
    , vars :: VarContext
    , input :: [String]
    , output :: [String]
    }

instance Show Context where
    show :: Context -> String
    show cxt = "Functions: " ++ show (funs cxt) ++ "\nVariables: " ++ show (vars cxt)

instance Eq Context where
    (==) :: Context -> Context -> Bool
    (==) c1 c2 = funs c1 == funs c2 && vars c1 == vars c2

empty :: Context
empty = Context
    { funs = FunContext
    , vars = emptyVarContext
    , input = []
    , output = []
    }


getVar :: Context -> String -> Maybe Int
getVar cxt var = 
    let mp = context . vars $ cxt in
    Map.lookup var mp

setVar :: Context -> String -> Int -> Context
setVar cxt name val = 
    let mp = context . vars $ cxt in
    cxt { vars = VarContext $ Map.insert name val mp }
