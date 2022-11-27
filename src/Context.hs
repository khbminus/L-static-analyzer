{-# LANGUAGE InstanceSigs #-}

module Context(Context(..), InputSource(..), newContext, getVar, setVar) where

import qualified Data.Map as Map

data FunContext = FunContext deriving (Show, Eq)

newtype VarContext = VarContext {context :: Map.Map String Int} deriving (Show, Eq)

data InputSource = InputSource {fileName :: String, inputLines :: [String]} deriving (Show)

emptyVarContext :: VarContext
emptyVarContext = VarContext {context = Map.empty}

data Context = Context
  { funs :: FunContext,
    vars :: VarContext,
    input :: InputSource,
    output :: [String]
  }
  deriving (Show)

instance Eq Context where
  (==) :: Context -> Context -> Bool
  (==) c1 c2 = funs c1 == funs c2 && vars c1 == vars c2

newContext :: InputSource -> Context
newContext i =
  Context
    { funs = FunContext,
      vars = emptyVarContext,
      input = i,
      output = []
    }

getVar :: Context -> String -> Maybe Int
getVar cxt var =
  let mp = context . vars $ cxt
   in Map.lookup var mp

setVar :: String -> Int -> Context -> Context
setVar name val ctx =
  let mp = context . vars $ ctx
   in ctx {vars = VarContext $ Map.insert name val mp}
