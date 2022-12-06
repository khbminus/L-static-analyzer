{-# LANGUAGE InstanceSigs #-}

module Context (Context (..), InputSource (..), newContext, getVar, setVar, getFun, setFun, loadFunStack, unloadFunStack) where

import qualified Data.Map as Map
import Error (RuntimeError)
import Statement (Function (..))

newtype FunContext = FunContext {funContext :: Map.Map String Function} deriving (Show, Eq)

newtype VarContext = VarContext {varContext :: Map.Map String Int} deriving (Show, Eq)

data InputSource = InputSource {fileName :: String, inputLines :: [String]} deriving (Show)

emptyVarContext :: VarContext
emptyVarContext = VarContext {varContext = Map.empty}

emptyFunContext :: FunContext
emptyFunContext = FunContext {funContext = Map.empty}

data Context = Context
  { funs :: [FunContext],
    vars :: [VarContext],
    error :: Maybe RuntimeError
  }
  deriving (Show)

instance Eq Context where
  (==) :: Context -> Context -> Bool
  (==) c1 c2 = funs c1 == funs c2 && vars c1 == vars c2

newContext :: Context
newContext =
  Context
    { funs = [emptyFunContext],
      vars = [emptyVarContext],
      Context.error = Nothing
    }

getHelper :: String -> [Map.Map String a] -> Maybe a
getHelper _ [] = Nothing
getHelper var (x : xs) = case Map.lookup var x of
  Nothing -> getHelper var xs
  j -> j

-- TODO: is some kind of Lens/type-class applicable this?

getVar :: String -> Context -> Maybe Int
getVar var ctx = getHelper var (map varContext (vars ctx))

setVar :: String -> Int -> Context -> Context
setVar name val ctx =
  let mp = varContext . head . vars $ ctx
   in let vc = VarContext $ Map.insert name val mp
       in ctx {vars = vc : (tail . vars) ctx}

getFun :: String -> Context -> Maybe Function
getFun var ctx = getHelper var (map funContext (funs ctx))

setFun :: String -> Function -> Context -> Context
setFun name f ctx =
  let mp = funContext . head . funs $ ctx
   in let fc = FunContext $ Map.insert name f mp
       in ctx {funs = fc : (tail . funs) ctx}

loadFunStack :: Function -> Context -> Context
loadFunStack Function {} ctx = ctx {funs = emptyFunContext : funs ctx, vars = emptyVarContext : vars ctx}

unloadFunStack :: Context -> Context
unloadFunStack ctx = ctx {funs = (tail . funs) ctx, vars = (tail . vars) ctx}
