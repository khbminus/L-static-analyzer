{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}

module Context where

import qualified Data.Map as Map
import Error (RuntimeError (FunctionNotFound, VarNotFound))
import Statement (Function (..))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.State
import Data.Foldable (Foldable(fold))
import Data.Maybe (fromJust, isJust)
import GHC.IO.Handle (hIsOpen)

newtype FunContext = FunContext {funContext :: Map.Map String Function} deriving (Show, Eq)

newtype VarContext = VarContext {varContext :: Map.Map String Int} deriving (Show, Eq)

data InputSource = InputSource {fileName :: String, inputLines :: [String]} deriving (Show)

newtype Buffer = Buffer [String] deriving (Show, Eq)

push :: String -> Buffer -> Buffer
push str (Buffer buf) = Buffer $ buf ++ [str]

pop :: Buffer -> (Buffer, Maybe String) 
pop (Buffer (x:xs)) = (Buffer xs, Just x)
pop (Buffer []) = (Buffer [], Nothing)

emptyVarContext :: VarContext
emptyVarContext = VarContext {varContext = Map.empty}

emptyFunContext :: FunContext
emptyFunContext = FunContext {funContext = Map.empty}

setVarContext :: String -> Int -> VarContext -> VarContext
setVarContext name val ctx =
  let mp = varContext ctx in 
  VarContext $ Map.insert name val mp

data Context = Context
  { funs :: [FunContext],
    vars :: [VarContext],
    error :: Maybe RuntimeError,
    input :: Buffer,
    output :: Buffer,
    flushEnabled :: Bool
  }
  deriving (Show)

pattern ErrorContext :: Context
pattern ErrorContext <- Context { Context.error = (Just _) }

instance Eq Context where
  (==) :: Context -> Context -> Bool
  (==) c1 c2 = funs c1 == funs c2 && vars c1 == vars c2

newContext :: Context
newContext =
  Context
    { funs = [emptyFunContext],
      vars = [emptyVarContext],
      Context.error = Nothing,
      input = Buffer [],
      output = Buffer [],
      flushEnabled = True
    }

getHelper :: String -> [Map.Map String a] -> Maybe a
getHelper _ [] = Nothing
getHelper var (x : xs) = case Map.lookup var x of
  Nothing -> getHelper var xs
  j -> j

-- TODO: is some kind of Lens/type-class applicable this?

getVar :: String -> Context -> Maybe Int
getVar var ctx = getHelper var (map varContext (vars ctx))

getVarT :: String -> MaybeT (StateT Context IO) Int
getVarT var = do
  cxt <- get
  case getVar var cxt of
    Nothing -> do { lift $ setErrorT $ VarNotFound var; mzero }
    Just v -> return v

setVar :: String -> Int -> Context -> Context
setVar name val ctx =
  let mp = varContext . head . vars $ ctx
   in let vc = VarContext $ Map.insert name val mp
       in ctx {vars = vc : (tail . vars) ctx}

getFun :: String -> Context -> Maybe Function
getFun fun ctx = getHelper fun (map funContext (funs ctx))

getFunT :: String -> MaybeT (StateT Context IO) Function
getFunT fun = do
  ctx <- get
  case getFun fun ctx of
    Nothing -> do { lift $ setErrorT $ FunctionNotFound fun; mzero }
    Just f -> return f

setError :: RuntimeError -> Context -> Context
setError err cxt = cxt { Context.error = Just err }

setErrorT :: RuntimeError -> StateT Context IO ()
setErrorT err = get >>= put . setError err

pushOutput :: String -> StateT Context IO ()
pushOutput str = do
  cxt <- get
  put $ cxt { output = push str (output cxt) }

popInput :: MaybeT (StateT Context IO) String
popInput = do
  cxt <- get
  let (buf, h) = pop $ input cxt
  ret <- maybe mzero return h
  put $ cxt { input = buf }
  return ret

popOutput :: MaybeT (StateT Context IO) String
popOutput = do
  cxt <- get
  let (buf, h) = pop $ output cxt
  ret <- maybe mzero return h
  put $ cxt { output = buf }
  return ret


setFun :: String -> Function -> Context -> Context
setFun name f ctx =
  let mp = funContext . head . funs $ ctx
   in let fc = FunContext $ Map.insert name f mp
       in ctx {funs = fc : (tail . funs) ctx}

loadFunStack :: Function -> [Int] -> Context -> Context
loadFunStack (Function args _ _) values ctx = ctx {funs = emptyFunContext : funs ctx, vars = insertAll (zip args values) emptyVarContext : vars ctx}
  where
    insertAll :: [(String, Int)] -> VarContext -> VarContext
    insertAll [] x = x
    insertAll ((name, value) : xs) (VarContext mp) = insertAll xs VarContext {varContext = Map.insert name value mp}

unloadFunStack :: Context -> Context
unloadFunStack ctx = ctx {funs = (tail . funs) ctx, vars = (tail . vars) ctx}

flush :: StateT Context IO ()
flush = do

  out <- runMaybeT popOutput
  when (isJust out) $ do
    lift $ putStrLn $ fromJust out
    flush
